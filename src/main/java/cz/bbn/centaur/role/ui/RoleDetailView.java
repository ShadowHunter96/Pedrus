package cz.bbn.cerberus.role.ui;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.permission.PermissionService;
import cz.bbn.cerberus.role.RoleService;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.role.dto.RoleHasPermissionDto;
import cz.bbn.cerberus.role.ui.component.RoleDetailComponent;
import lombok.extern.slf4j.Slf4j;

import java.util.HashSet;
import java.util.Set;

@Route(value = RoleDetailView.ROUTE, layout = MainLayout.class)
@Slf4j
public class RoleDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "role-detail";

    private final RoleService roleService;
    private final PermissionService permissionService;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public RoleDetailView(RoleService roleService, PermissionService permissionService, AppEnv appEnv,
                          EntityNewComponentOperation entityNewComponentOperation) {
        this.roleService = roleService;
        this.permissionService = permissionService;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
        setSizeFull();
    }

    protected void initView(RoleDto roleDto) {
        removeAll();
        Set<RoleHasPermissionDto> allPermissions = permissionService.convertPermissionsToDto();
        Set<RoleHasPermissionDto> permissionLeftList = new HashSet<>(allPermissions);
        Set<RoleHasPermissionDto> permissionRightList = new HashSet<>();
        if (roleDto.getId() != null) {
            roleDto.getRoleHasPermissionSet().forEach(roleDtoActual ->
                    permissionLeftList.removeIf(dto -> roleDtoActual.getPermissionId().equals(dto.getPermissionId()))
            );
            permissionRightList.addAll(roleDto.getRoleHasPermissionSet());
        }

        RoleDetailComponent roleDetailComponent = new RoleDetailComponent(
                roleDto,
                getSaveAction(permissionRightList),
                SecurityUtils.hasPermission(Permission.ROLE_EDIT),
                RoleView.ROUTE,
                appEnv,
                entityNewComponentOperation
        );
        roleDetailComponent.initComponent();
        this.add(roleDetailComponent);
    }

    private SaveAction<RoleDto> getSaveAction(Set<RoleHasPermissionDto> permissionRightList) {
        return (dto, originalDto) -> {
            dto.setRoleHasPermissionSet(new HashSet<>(permissionRightList.stream().toList()));
            try {
                if (roleService.roleExists(dto.getId())) {
                    roleService.updateRole(dto, originalDto);
                } else {
                    roleService.saveRole(dto);
                }
                SuccessNotification.showSavingSuccess(appEnv);
                UI.getCurrent().navigate(RoleView.ROUTE);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String param) {
        if (param != null) {
            try {
                RoleDto roleDto = roleService.getRole(param);
                refreshBreadcrumbText(roleDto.getId());
                initView(roleDto);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        } else {
            initView(new RoleDto());
        }
    }
}
