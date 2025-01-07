package cz.bbn.cerberus.approvement.ui;

import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.approvement.ApprovementComponentOperation;
import cz.bbn.cerberus.approvement.ui.component.ApprovementFilterComponent;
import cz.bbn.cerberus.approvement.ui.component.ApprovementGridComponent;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.FilterAction;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.enumeration.EnumerationComponentOperation;
import cz.bbn.cerberus.holiday.HolidayService;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.role.RoleService;
import cz.bbn.cerberus.translation.Transl;

import java.time.LocalDate;

@Route(value = ApprovementApproverView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.APPROVEMENT_APROVER_VIEW)
public class ApprovementApproverView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "approvement-approver-list";

    private final AppEnv appEnv;
    private final ApprovementComponentOperation approvementComponentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final ListService listService;
    private final HolidayService holidayService;
    private final RoleService roleService;
    private final EnumerationComponentOperation enumerationComponentOperation;

    private ApprovementGridComponent grid;

    public ApprovementApproverView(AppEnv appEnv, ApprovementComponentOperation approvementComponentOperation,
                                   EntityNewComponentOperation entityNewComponentOperation, ListService listService,
                                   HolidayService holidayService, RoleService roleService,
                                   EnumerationComponentOperation enumerationComponentOperation) {
        this.appEnv = appEnv;
        this.approvementComponentOperation = approvementComponentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.listService = listService;
        this.holidayService = holidayService;
        this.roleService = roleService;
        this.enumerationComponentOperation = enumerationComponentOperation;
    }

    private void initView(String params) {
        removeAll();
        setSizeFull();

        ComboBox<Integer> year = new ComboBox<>(Transl.get("Year"));
        year.setItems(AppUtils.getYears(10));
        year.setValue(LocalDate.now().getYear());

        ApprovementFilterComponent approvementFilterComponent =
                new ApprovementFilterComponent(params, listService.getEmployeeDtoListForApprover(),
                        getFilterAction(), getHistoryBreadcrumbs(), year, ROUTE,
                        SecurityUtils.hasPermission(Permission.APPROVEMENT_VIEW_ALL));

        EmployeeDto userEmployeeDto = listService.getUserDtoList().stream().filter(actualUserDto ->
                actualUserDto.getId().equals(SecurityUtils.getCurrentUserId())).findAny().get().getEmployee();
        grid = new ApprovementGridComponent(appEnv,
                approvementComponentOperation.getItemsAction(approvementFilterComponent), approvementComponentOperation,
                listService, null, roleService.findAll(), null, null
                , userEmployeeDto, enumerationComponentOperation, holidayService.findAll(), year);

        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Approvement"), entityNewComponentOperation,
                NoteTypeEnum.ANY, ObjectType.ANY);
        card.setId(RobotFrameworkVariables.APPROVEMENT_CARD_VIEW_CARD_ID.getValue());
        card.add(approvementFilterComponent);
        card.add(grid);

        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation));
        add(card);
        grid.loadData();
    }

    private FilterAction getFilterAction() {
        return () -> grid.loadData();
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String params) {
        initView(params);
    }
}
