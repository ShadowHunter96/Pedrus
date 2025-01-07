package cz.bbn.cerberus.approvement.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.html.H3;
import com.vaadin.flow.component.icon.VaadinIcon;
import cz.bbn.cerberus.approvement.ApprovementComponentOperation;
import cz.bbn.cerberus.approvement.enums.ApprovementType;
import cz.bbn.cerberus.approvement.ui.ApprovementHolidayView;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.interfaces.CountActionDouble;
import cz.bbn.cerberus.commons.component.ui.interfaces.FilterAction;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.enumeration.EnumerationComponentOperation;
import cz.bbn.cerberus.holiday.HolidayService;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.role.RoleService;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;

import java.time.LocalDate;

public class ApprovementCardComponent {

    private final AppEnv appEnv;
    private final ApprovementComponentOperation approvementComponentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final ListService listService;
    private final HolidayService holidayService;
    private final HistoryBreadcrumbs historyBreadcrumbs;
    private final FilterAction filterAction;
    private final ApprovementType approvementType;
    private final String params;
    private final Permission permission;
    private final RoleService roleService;
    private final String route;
    private final EnumerationComponentOperation enumerationComponentOperation;

    private H3 daysLabel;
    private ApprovementGridComponent grid;

    public ApprovementCardComponent(AppEnv appEnv, ApprovementComponentOperation approvementComponentOperation,
                                    EntityNewComponentOperation entityNewComponentOperation, ListService listService,
                                    HolidayService holidayService, HistoryBreadcrumbs historyBreadcrumbs,
                                    FilterAction filterAction, ApprovementType approvementType, String params,
                                    Permission permission, RoleService roleService, String route,
                                    EnumerationComponentOperation enumerationComponentOperation) {
        this.appEnv = appEnv;
        this.approvementComponentOperation = approvementComponentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.listService = listService;
        this.holidayService = holidayService;
        this.historyBreadcrumbs = historyBreadcrumbs;
        this.filterAction = filterAction;
        this.approvementType = approvementType;
        this.params = params;
        this.permission = permission;
        this.roleService = roleService;
        this.route = route;
        this.enumerationComponentOperation = enumerationComponentOperation;
    }

    public AppCardGridComponent getCard() {
        UserDto userDto = listService.getUserDtoList().stream().filter(actualUserDto ->
                actualUserDto.getId().equals(SecurityUtils.getCurrentUserId())).findFirst().orElse(null);
        EmployeeDto employeeDto = userDto == null && userDto.getEmployee() == null ? null : userDto.getEmployee();
        AppCardGridComponent card;
        boolean master = SecurityUtils.hasPermission(Permission.APPROVEMENT_VIEW_ALL);
        if (employeeDto == null && !master) {
            ErrorNotification.show(Transl.get("User does not have an assigned employee"), appEnv);
            card = new AppCardGridComponent(Transl.get(approvementType.name()), entityNewComponentOperation);
            card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation));
        } else if (employeeDto != null && employeeDto.getSuperiorUserDto() == null && !master) {
            ErrorNotification.show(Transl.get("User does not have assigned a superior"), appEnv);
            card = new AppCardGridComponent(
                    Transl.get(Transl.get(approvementType.name())), entityNewComponentOperation);
            card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation));
        } else {
            ComboBox<Integer> year = new ComboBox<>(Transl.get("Year"));
            year.setItems(AppUtils.getYears(10));
            year.setValue(LocalDate.now().getYear());
            getCountActionDouble(year).getCount();
            daysLabel.setClassName("days-label");
            ApprovementFilterComponent approvementFilterComponent =
                    new ApprovementFilterComponent(params, listService.getEmployeeDtoList(),
                            filterAction, historyBreadcrumbs, approvementType, employeeDto,
                            getCountActionDouble(year), year, route,
                            approvementComponentOperation.isBackOffice(approvementType, roleService.findAll()),
                            master);

            grid = new ApprovementGridComponent(appEnv,
                    approvementComponentOperation.getItemsAction(approvementFilterComponent),
                    approvementComponentOperation, listService, getCountActionDouble(year),
                    approvementType == ApprovementType.PAID_LEAVE || approvementType == ApprovementType.ILL ? roleService.findAll() : null, approvementType,
                    listService.getEmployeeDtoList(), employeeDto, enumerationComponentOperation, holidayService.findAll(), year);

            Button button = null;
            if (SecurityUtils.hasPermission(Permission.APPROVEMENT_EDIT_ALL)) {
                button = VaadinComponents.getButton(Transl.get("New request for employee"), VaadinIcon.PLUS.create());
                button.addClickListener(buttonClickEvent ->
                        approvementComponentOperation.getNewAprovementDialogEvent(
                                        grid, approvementType, userDto, getCountActionDouble(year), true)
                                .onComponentEvent(buttonClickEvent)
                );
            }
            card = new AppCardGridComponent(Transl.get(approvementType.name()),
                    (approvementType.equals(ApprovementType.ILL)
                            && !approvementComponentOperation.isBackOffice(approvementType, roleService.findAll()))
                            || employeeDto == null ? null : permission,
                    Transl.get("New".concat(" ").concat(approvementType.name())),
                    approvementComponentOperation.getNewAprovementDialogEvent(
                            grid, approvementType, userDto, getCountActionDouble(year), false),
                    entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY, button);
            card.add(approvementFilterComponent);
            card.add(grid);
            card.addToHeader(daysLabel);

            card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation));
            card.setId(RobotFrameworkVariables.APPROVEMENT_CARD_VIEW_CARD_ID.getValue());
            grid.loadData();
        }
        return card;
    }

    public CountActionDouble getCountActionDouble(ComboBox<Integer> year) {
        return () -> {
            Double count = approvementComponentOperation.getApprovementDaysCountAction(year, approvementType)
                    .getCount();
            if (daysLabel == null) {
                daysLabel = new H3();
            }
            switch (approvementType) {
                case HOME_OFFICE, UNPAID_LEAVE, PAID_LEAVE, BUSSINES_TRIP ->
                        daysLabel.setText(Transl.get("Days").concat(": ")
                                .concat(String.valueOf(count.intValue())));
                case HOLIDAY -> daysLabel.setText(Transl.get("Days").concat(": ")
                        .concat(count % 1 == 0 ? String.valueOf(count.intValue()) : String.valueOf(count)).concat("/")
                        .concat(String.valueOf(ApprovementHolidayView.HOLIDAY_LIMIT.intValue()))
                        .concat(" - ")
                        .concat(Transl.get("Hours"))
                        .concat(": ")
                        .concat(count % 1 == 0 ? String.valueOf((count.intValue() * 8)) : String.valueOf((count * 8)))
                        .concat("/")
                        .concat(String.valueOf(ApprovementHolidayView.HOLIDAY_LIMIT.intValue() * 8)));
                default -> {
                }
            }

            return count;
        };
    }

    public ApprovementGridComponent getGrid() {
        return grid;
    }

}
