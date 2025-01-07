package cz.bbn.cerberus.virtualserver;

import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.grid.ItemDoubleClickEvent;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.ConfirmAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.enumeration.EnumerationService;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.role.RoleService;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.virtualserver.dto.HddDto;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerAction;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerDto;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerFilterDto;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerStatus;
import cz.bbn.cerberus.virtualserver.ui.component.VirtualServerDialog;
import cz.bbn.cerberus.virtualserver.ui.component.VirtualServerFilterComponent;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Component
@Slf4j
public class VirtualServerComponentOperation {

    private final VirtualServerService virtualServerService;
    private final ListService listService;
    private final UserService userService;
    private final RoleService roleService;
    private final EnumerationService enumerationService;
    private final AppEnv appEnv;

    public VirtualServerComponentOperation(VirtualServerService virtualServerService,
                                           ListService listService, UserService userService, RoleService roleService,
                                           EnumerationService enumerationService, AppEnv appEnv) {
        this.virtualServerService = virtualServerService;
        this.listService = listService;
        this.userService = userService;
        this.roleService = roleService;
        this.enumerationService = enumerationService;
        this.appEnv = appEnv;
    }

    public List<UserDto> findUserList() {
        return listService.getUserDtoList();
    }

    public ItemsAction<VirtualServerDto> getItemsAction(VirtualServerFilterComponent virtualServerFilterComponent) {
        return (query, orderList) -> {
            VirtualServerFilterDto virtualServerFilterDto = virtualServerFilterComponent.getVirtualServerFilterDto();
            virtualServerFilterDto.setPage(query.getPage());
            virtualServerFilterDto.setSize(query.getPageSize());
            virtualServerFilterDto.setOrderList(orderList);
            return virtualServerService.findVirtualServerDtoPage(virtualServerFilterDto);
        };
    }

    public DeleteAction getDeleteAction() {
        return id -> {
            try {
                virtualServerService.deleteVirtualServer(Long.valueOf(id));
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    public boolean getIsInfrastructure() {
        Set<String> roleSet = new HashSet<>();
        RoleDto infrastructureRole = new RoleDto();
        try {
            roleSet = SecurityUtils.getCurrentUser().getActiveRoleSet();
            infrastructureRole = roleService.getInfrastructureRole();
        } catch (SystemException e) {
            log.error(e.getMessage(), e);
            ErrorNotification.show(Transl.get(e.getMessage()), appEnv);
        }
        if (infrastructureRole.getId() != null && roleSet != null) {
            for (String activeRole : roleSet) {
                if (activeRole.equals(infrastructureRole.getId())) {
                    return true;
                }
            }
        }
        return false;
    }

    public ComponentEventListener<ClickEvent<? extends com.vaadin.flow.component.Component>> getNewVirtualServerEvent(
            AppInfiniteGrid<VirtualServerDto> grid) {
        return buttonClickEvent -> {
            VirtualServerDto dto = new VirtualServerDto();
            VirtualServerDialog dialog = new VirtualServerDialog(
                    dto, enumerationService.getEnumerationDtoListByTypeNotDeletedAllowed("SUBNET"), this, grid);
            dialog.open();
        };
    }

    public ComponentEventListener<ItemDoubleClickEvent<VirtualServerDto>> getEditVirtualServerEvent(
            AppInfiniteGrid<VirtualServerDto> grid) {
        return event -> {
            VirtualServerDialog dialog = new VirtualServerDialog(event.getItem(),
                    enumerationService.getEnumerationDtoListByTypeNotDeletedAllowed("SUBNET"), this, grid);
            dialog.open();
        };
    }

    public void cancelRequest(VirtualServerDto dto, AppDialog dialog, AppInfiniteGrid<VirtualServerDto> grid) {
        try {
            dto.setStatus(VirtualServerStatus.DELETED);
            dto.setDeleted(true);
            virtualServerService.updateVirtualServer(dto, VirtualServerAction.CANCEL_REQUEST);
            dialog.close();
            grid.loadData();
            SuccessNotification.showSavingSuccess(appEnv);
        } catch (SystemException e) {
            log.error(TextValues.SYSTEM_EXCEPTION, e);
            ErrorNotification.show(e.getMessage(), appEnv);
        }
    }

    public void requestDeleting(VirtualServerDto dto, AppDialog dialog, AppInfiniteGrid<VirtualServerDto> grid) {
        try {
            dto.setStatus(VirtualServerStatus.DELETING);
            virtualServerService.updateVirtualServer(dto, VirtualServerAction.REQUEST_DELETING);
            dialog.close();
            grid.loadData();
            SuccessNotification.showSavingSuccess(appEnv);
        } catch (SystemException e) {
            log.error(TextValues.SYSTEM_EXCEPTION, e);
            ErrorNotification.show(e.getMessage(), appEnv);
        }
    }

    public void save(Binder<VirtualServerDto> binder, List<HddDto> hddList, List<Binder<HddDto>> binderList,
                     VirtualServerStatus status, AppDialog dialog, AppInfiniteGrid<VirtualServerDto> grid) {
        boolean allOk = true;
        for (Binder<HddDto> hddBinder : binderList) {
            if (!hddBinder.validate().isOk()) {
                allOk = false;
            }
        }
        if (binder.validate().isOk() && allOk) {
            save(binder.getBean(), hddList, dialog, grid, status, null);
        }
    }

    public void create(Binder<VirtualServerDto> binder, List<HddDto> hddList, List<Binder<HddDto>> binderList,
                       VirtualServerDialog dialog, AppInfiniteGrid<VirtualServerDto> grid) {
        boolean allOk = true;
        for (Binder<HddDto> hddBinder : binderList) {
            if (!hddBinder.validate().isOk()) {
                allOk = false;
            }
        }
        if (binder.validate().isOk() && allOk) {
            VirtualServerDto dto = binder.getBean();
            dto.setCreationDate(LocalDateTime.now());
            save(binder.getBean(), hddList, dialog, grid, VirtualServerStatus.RUNNING, VirtualServerAction.CREATE);
        }
    }

    public void delete(VirtualServerDto dto, VirtualServerDialog dialog,
                       AppInfiniteGrid<VirtualServerDto> grid) {
        try {
            virtualServerService.deleteVirtualServer(dto.getId());
            if (dialog != null) {
                dialog.close();
            }
            grid.loadData();
            SuccessNotification.showSavingSuccess(appEnv);
        } catch (SystemException e) {
            log.error(TextValues.SYSTEM_EXCEPTION, e);
            ErrorNotification.show(e.getMessage(), appEnv);
        }
    }

    public ConfirmAction getDeleteConfirmAction(VirtualServerDto dto, AppInfiniteGrid<VirtualServerDto> grid) {
        return () -> delete(dto, null, grid);
    }

    private void save(VirtualServerDto dto, List<HddDto> hddList, AppDialog dialog,
                      AppInfiniteGrid<VirtualServerDto> grid, VirtualServerStatus status, VirtualServerAction action) {
        try {
            dto.setHddDtoList(hddList);
            dto.setStatus(status);
            if (dto.getId() == null) {
                virtualServerService.createVirtualServer(dto);
            } else {
                virtualServerService.updateVirtualServer(dto, action);
            }
            dialog.close();
            grid.loadData();
            SuccessNotification.showSavingSuccess(appEnv);
        } catch (SystemException e) {
            log.error(TextValues.SYSTEM_EXCEPTION, e);
            ErrorNotification.show(e.getMessage(), appEnv);
        }
    }

    public void cancelDeleting(VirtualServerDto dto, VirtualServerDialog dialog,
                               AppInfiniteGrid<VirtualServerDto> grid) {
        try {
            dto.setStatus(VirtualServerStatus.RUNNING);
            virtualServerService.updateVirtualServer(dto, VirtualServerAction.CANCEL_REQUEST);
            dialog.close();
            grid.loadData();
            SuccessNotification.showSavingSuccess(appEnv);
        } catch (SystemException e) {
            log.error(TextValues.SYSTEM_EXCEPTION, e);
            ErrorNotification.show(e.getMessage(), appEnv);
        }
    }
}
