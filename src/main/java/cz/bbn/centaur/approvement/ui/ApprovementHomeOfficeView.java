package cz.bbn.cerberus.approvement.ui;

import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.approvement.ApprovementComponentOperation;
import cz.bbn.cerberus.approvement.enums.ApprovementType;
import cz.bbn.cerberus.approvement.ui.component.ApprovementCardComponent;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.FilterAction;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.enumeration.EnumerationComponentOperation;
import cz.bbn.cerberus.holiday.HolidayService;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.role.RoleService;


@Route(value = ApprovementHomeOfficeView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.APPROVEMENT_HOME_OFFICE_VIEW)
public class ApprovementHomeOfficeView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "approvement-homeoffice-list";

    private final AppEnv appEnv;
    private final ApprovementComponentOperation approvementComponentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final ListService listService;
    private final HolidayService holidayService;
    private final RoleService roleService;
    private final EnumerationComponentOperation enumerationComponentOperation;

    private ApprovementCardComponent approvementCardComponent;

    public ApprovementHomeOfficeView(AppEnv appEnv, ApprovementComponentOperation approvementComponentOperation,
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

        removeAll();
        setSizeFull();

        approvementCardComponent = new ApprovementCardComponent(appEnv, approvementComponentOperation,
                entityNewComponentOperation, listService, holidayService, getHistoryBreadcrumbs(), getFilterAction(),
                ApprovementType.HOME_OFFICE, params, Permission.APPROVEMENT_HOME_OFFICE_VIEW, roleService, ROUTE,
                enumerationComponentOperation);
        AppCardGridComponent card = approvementCardComponent.getCard();
        add(card);
    }

    private FilterAction getFilterAction() {
        return () -> approvementCardComponent.getGrid().loadData();
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String params) {
        initView(params);
    }
}
