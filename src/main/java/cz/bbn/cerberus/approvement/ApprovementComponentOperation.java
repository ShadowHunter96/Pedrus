package cz.bbn.cerberus.approvement;

import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.approvement.dto.ApprovementBusinessTripDto;
import cz.bbn.cerberus.approvement.dto.ApprovementDto;
import cz.bbn.cerberus.approvement.dto.ApprovementFilterDto;
import cz.bbn.cerberus.approvement.dto.ApprovementSimpleDto;
import cz.bbn.cerberus.approvement.enums.ApprovementState;
import cz.bbn.cerberus.approvement.enums.ApprovementType;
import cz.bbn.cerberus.approvement.persistance.entity.ApprovementDayEntity;
import cz.bbn.cerberus.approvement.ui.ApprovementHolidayView;
import cz.bbn.cerberus.approvement.ui.component.ApprovementConfirmDialog;
import cz.bbn.cerberus.approvement.ui.component.ApprovementDetailComponent;
import cz.bbn.cerberus.approvement.ui.component.ApprovementDialog;
import cz.bbn.cerberus.approvement.ui.component.ApprovementFilterComponent;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.CountActionDouble;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.enumeration.EnumerationComponentOperation;
import cz.bbn.cerberus.holiday.HolidayService;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.role.RoleService;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

@Component
@Slf4j
public class ApprovementComponentOperation {

    private final ApprovementService approvementService;
    private final AppEnv appEnv;
    private final ListService listService;
    private final HolidayService holidayService;
    private final RoleService roleService;
    private final EnumerationComponentOperation enumerationComponentOperation;

    public ApprovementComponentOperation(ApprovementService approvementService, AppEnv appEnv,
                                         ListService listService, HolidayService holidayService,
                                         RoleService roleService,
                                         EnumerationComponentOperation enumerationComponentOperation) {
        this.approvementService = approvementService;
        this.appEnv = appEnv;
        this.listService = listService;
        this.holidayService = holidayService;
        this.roleService = roleService;
        this.enumerationComponentOperation = enumerationComponentOperation;
    }

    public ItemsAction<ApprovementSimpleDto> getItemsAction(ApprovementFilterComponent approvementFilterComponent) {
        return (query, orderList) -> {
            ApprovementFilterDto filter = approvementFilterComponent.getApprovementFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return approvementService.findApprovementDtoPage(filter);
        };
    }

    public CountActionDouble getApprovementDaysCountAction(ComboBox<Integer> year, ApprovementType approvementType) {
        return () -> approvementService.getCountByType(approvementType, year.getValue());
    }

    public ComponentEventListener<
            ClickEvent<? extends com.vaadin.flow.component.Component>> getNewAprovementDialogEvent(AppInfiniteGrid<ApprovementSimpleDto> grid,
                                          ApprovementType type, UserDto userDto,
                                          CountActionDouble countActionDouble,
                                          boolean master) {
        return buttonClickEvent -> {
            try {
                ApprovementDto dto = new ApprovementDto();
                dto.setApprovementType(type);
                dto.setOwnerUserDto(userDto);
                dto.setCreatedUserDto(userDto);
                dto.setHalfDay(false);
                dto.setApprovementProjectEmployeeDtoList(new ArrayList<>());
                dto.setLineManageApproved(false);
                dto.setSuperiorApproved(false);
                if (type == ApprovementType.BUSSINES_TRIP) {
                    ApprovementBusinessTripDto approvementBusinessTripDto = new ApprovementBusinessTripDto();
                    dto.setApprovementBusinessTripDto(approvementBusinessTripDto);
                }
                if (dto.getApprovementType() == ApprovementType.PAID_LEAVE
                        || dto.getApprovementType() == ApprovementType.ILL) {
                    RoleDto roleDto = roleService.getByBackOffice();
                    dto.setLineManagerRole(roleDto);
                } else {
                    dto.setLineManagerUserDto(userDto.getEmployee().getLineManagerUserDto());
                }
                if (dto.getApprovementType() != ApprovementType.PAID_LEAVE
                        && dto.getApprovementType() != ApprovementType.ILL
                        && dto.getLineManagerUserDto() == null) {
                    dto.setApprovementState(ApprovementState.WAITING_FOR_SUPERIOR);
                } else if (dto.getApprovementType() == ApprovementType.ILL) {
                    dto.setApprovementState(ApprovementState.COMPLETELY_APPROVED);
                } else {
                    dto.setApprovementState(ApprovementState.WAIT_FOR_LM_MANAGER);
                }

                dto.setSuperiorUserDto(userDto.getEmployee().getSuperiorUserDto());
                dto.setDays(0D);
                EmployeeDto userEmployeeDto = listService.getUserDtoList().stream().filter(userDto1 ->
                        userDto1.getId().equals(SecurityUtils.getCurrentUserId())).findAny().get().getEmployee();
                ApprovementDialog approvementDialog = new ApprovementDialog(type, grid, this,
                        dto, listService.getAllowedProjectDtoList(), listService.getEmployeeDtoList(),
                        holidayService.findAll(), countActionDouble,
                        dto.getApprovementType() == ApprovementType.PAID_LEAVE
                                || dto.getApprovementType() == ApprovementType.ILL ? roleService.findAll() : null,
                        listService.getSubjectDtoList(), listService.getOpportunityDtoList(),
                        userEmployeeDto, enumerationComponentOperation, master);
                approvementDialog.open();
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public ApprovementDto getApprovementDto(Long id) {
        try {
            return approvementService.getApprovementDto(id);
        } catch (SystemException ex) {
            log.error(TextValues.SYSTEM_EXCEPTION, ex);
            ErrorNotification.show(ex, appEnv);
        }
        return null;
    }

    public SaveAction<ApprovementDto> getSaveAction(AppDialog appDialog,
                                                    boolean master, Binder<ApprovementDto> binder,
                                                    ApprovementDetailComponent approvementDetailComponent,
                                                    AppInfiniteGrid<ApprovementSimpleDto> grid,
                                                    CountActionDouble countActionDouble) {
        return getSaveAction(appDialog, master, true, binder,
                approvementDetailComponent, grid, countActionDouble);
    }

    public SaveAction<ApprovementDto> getSaveAction(AppDialog appDialog,
                                                    boolean master, boolean showConfirmDialog, Binder<ApprovementDto> binder,
                                                    ApprovementDetailComponent approvementDetailComponent,
                                                    AppInfiniteGrid<ApprovementSimpleDto> grid,
                                                    CountActionDouble countActionDouble) {
        return (newDto, originalDto) -> {
            try {
                boolean valid = approvementValidate(appDialog, approvementDetailComponent.getProjectLayout(), newDto,
                        master, showConfirmDialog, binder, approvementDetailComponent, grid, countActionDouble);
                if (!valid) {
                    return;
                }
                List<String[]> list = newDto.getApprovementType() != ApprovementType.BUSSINES_TRIP
                        && newDto.getApprovementType() != ApprovementType.ILL
                        ? getApprovementProjectEmployeeDtoList(approvementDetailComponent.getProjectLayout(),
                        newDto.getApprovementType()) : new ArrayList<>();
                if (newDto.getId() == null) {
                    newDto.setCreated(LocalDateTime.now());
                    if (!master) {
                        checkLMManagerHoliday(newDto);
                    }
                    newDto.setCreated(LocalDateTime.now());
                    approvementService.saveApprovement(newDto, list, holidayService.findAll(), master);
                } else {
                    approvementService.updateApprovement(newDto, originalDto, list, holidayService.findAll(), master);
                }
                appDialog.showWarning(false);
                appDialog.close();
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    private void fillOwnerUserDto(ApprovementDto dto) {
        UserDto ownerUserDto = listService.getUserDtoList()
                .stream()
                .filter(userDto -> {
                    if (userDto.getEmployee() != null && dto.getFirstEmployeeDto() != null) {
                        return userDto.getEmployee()
                                .getId()
                                .equals(dto.getFirstEmployeeDto().getId());
                    }
                    return false;
                }).findAny().orElse(null);
        dto.setOwnerUserDto(ownerUserDto);
    }


    private void checkLMManagerHoliday(ApprovementDto dto) {
        List<ApprovementType> approvementTypeList = Arrays.stream(ApprovementType.values())
                .filter(approvementType -> approvementType != ApprovementType.HOME_OFFICE &&
                        approvementType != ApprovementType.BUSSINES_TRIP).collect(Collectors.toList());
        if (dto.getLineManagerUserDto() != null && approvementService.userApprovementCount(
                dto.getLineManagerUserDto().getId(), approvementTypeList, LocalDate.now()) > 0) {
            dto.setApprovementState(ApprovementState.WAITING_FOR_SUPERIOR);
        }
    }

    private boolean approvementValidate(AppDialog appDialog, HorizontalLayout projectLayout,
                                        ApprovementDto dto, boolean master, boolean showDialog,
                                        Binder<ApprovementDto> binder,
                                        ApprovementDetailComponent approvementDetailComponent,
                                        AppInfiniteGrid<ApprovementSimpleDto> grid,
                                        CountActionDouble countActionDouble) {
        boolean valid = true;

        if (dto.getId() == null) {
            if (dto.getApprovementType() == ApprovementType.ILL) {
                fillOwnerUserDto(dto);
                if (dto.getOwnerUserDto() == null) {
                    ErrorNotification.show(Transl.get("Employee have to be assigned some user"), appEnv);
                    valid = false;
                }
            }
        }

        if(master && dto.getApprovementState() != ApprovementState.COMPLETELY_APPROVED
                && dto.getApprovementState() != ApprovementState.DENIED
                && dto.getApprovementState() != ApprovementState.CANCELED
                && LocalDate.now().isBefore(dto.getDateFrom())) {

            dto.setApprovementState(ApprovementState.WAIT_FOR_LM_MANAGER);
        }else if (master && (LocalDate.now().isAfter(dto.getDateFrom()) || LocalDate.now().isEqual(dto.getDateFrom()))){
            dto.setSuperiorUserDto(SecurityUtils.getCurrentUserDto());
        }

        if (master && dto.getApprovementType() != ApprovementType.ILL && dto.getId() == null) {
            UserDto ownerUserDto = listService.getUserDtoList()
                    .stream()
                    .filter(userDto -> {
                                if (userDto.getEmployee() != null) {
                                    return userDto.getEmployee()
                                            .getId()
                                            .equals(dto.getCreatedForEmployeeDto().getId());
                                }
                                return false;
                            }
                    ).findAny().orElse(null);
            dto.setOwnerUserDto(ownerUserDto);
            if (ownerUserDto == null) {
                ErrorNotification.show(Transl.get("Employee have to be assigned some user"), appEnv);
                valid = false;
            }else{
                dto.setSuperiorUserDto(ownerUserDto.getEmployee().getSuperiorUserDto());
                dto.setLineManagerUserDto(ownerUserDto.getEmployee().getLineManagerUserDto());
            }
        }

        if (dto.getApprovementType() == ApprovementType.HOLIDAY && !master) {
            Double count = dto.getId() == null ?
                    approvementService.getCountByType(
                            dto.getApprovementType(), dto.getDateFrom().getYear()) :
                    approvementService.getCountByType(dto.getApprovementType(),
                            dto.getDateFrom().getYear(), dto.getId());
            if (count + dto.getDays() > ApprovementHolidayView.HOLIDAY_LIMIT) {
                ErrorNotification.show(Transl.get("The maximum number of " +
                        ApprovementHolidayView.HOLIDAY_LIMIT.intValue() + " vacation days has been exceeded."), appEnv);
                valid = false;
            }
        }
        if (dto.getDays() == 0) {
            ErrorNotification.show(Transl.get("Request cannot have zero days"), appEnv);
            valid = false;
        }

        if (dto.getApprovementType() != ApprovementType.ILL && !master) {
            ApprovementDayEntity approvementDayEntity;
            if (dto.getId() == null) {
                approvementDayEntity = approvementService.getApprovementByDate(dto.getDateFrom(),
                        dto.getDateTo(), dto.getOwnerUserDto().getId());
            } else {
                approvementDayEntity = approvementService.getApprovementByDate(dto.getDateFrom(),
                        dto.getDateTo(), dto.getOwnerUserDto().getId(), dto.getId());
            }

            if (approvementDayEntity != null && dto.getApprovementState() != ApprovementState.DENIED
                    && dto.getApprovementState() != ApprovementState.CANCELED) {
                ErrorNotification.show(Transl.get("Another request is already entered in this period")
                        .concat(" - ").concat(Transl.get(approvementDayEntity.getApprovementEntity()
                                .getApprovementType().name())), appEnv);
                valid = false;
            }
        } else if (dto.getId() == null && valid && dto.getApprovementType() == ApprovementType.ILL) {
            ApprovementDayEntity approvementDayEntity = approvementService.getApprovementByDate(dto.getDateFrom(), dto.getDateTo(),
                    dto.getOwnerUserDto().getId());
            if (approvementDayEntity != null && showDialog) {
                ApprovementConfirmDialog dialog = new ApprovementConfirmDialog(this,
                        approvementDayEntity.getApprovementEntity().getApprovementType(), appDialog, master, dto,
                        binder, approvementDetailComponent, grid, countActionDouble);
                dialog.open();
                valid = false;
            }
        }

        return valid;
    }

    public boolean isBackOffice(ApprovementType approvementType, List<RoleDto> roleList) {
        boolean hasBackOffice = false;
        if ((approvementType == ApprovementType.ILL || approvementType == ApprovementType.PAID_LEAVE)
                && roleList != null) {
            List<RoleDto> actualRoleDtoList = roleList.stream().filter(role ->
                    SecurityUtils.getCurrentUser().getActiveRoleSet().contains(role.getId())).toList();
            hasBackOffice = !actualRoleDtoList.stream().filter(
                    roleDto -> Boolean.TRUE.equals(roleDto.getBackOffice())).collect(Collectors.toList()).isEmpty();
        }
        return hasBackOffice;
    }

    private List<String[]> getApprovementProjectEmployeeDtoList(HorizontalLayout projectLayout,
                                                                ApprovementType approvementType) {
        List<String[]> list = new ArrayList<>();
        projectLayout.getChildren().toList().forEach(component -> {
            if (component instanceof HorizontalLayout) {
                ComboBox<ProjectDto> project =
                        (ComboBox<ProjectDto>) ((HorizontalLayout) component).getComponentAt(0);
                String[] values = new String[2];
                if (project.getValue() != null) {
                    values[0] = project.getValue().getId();
                }
                if (approvementType.isEmployeeMandatory()) {
                    ComboBox<EmployeeDto> employee =
                            (ComboBox<EmployeeDto>) ((HorizontalLayout) component).getComponentAt(1);
                    if (employee.getValue() != null) {
                        values[1] = employee.getValue().getId();
                    }
                }
                if (values[0] != null) {
                    list.add(values);
                }
            }
        });
        return list;
    }

}
