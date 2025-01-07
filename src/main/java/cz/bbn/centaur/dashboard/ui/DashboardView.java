package cz.bbn.cerberus.dashboard.ui;

import com.vaadin.flow.router.Route;
import com.vaadin.flow.router.RouteAlias;
import cz.bbn.cerberus.azure.AzureGraphService;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.dashboard.SalesDashboardComponentOperation;
import cz.bbn.cerberus.dashboard.ui.component.DashboardTab;
import cz.bbn.cerberus.dashboard.ui.component.DashboardTabsComponent;
import cz.bbn.cerberus.dashboard.ui.component.SalesDashboardTab;
import cz.bbn.cerberus.holiday.HolidayService;
import cz.bbn.cerberus.invoice.InvoicingService;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.management.ManagementComponentOperation;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.ProjectService;
import cz.bbn.cerberus.task.TaskService;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.workreport.WorkReportComponentOperation;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;

@Route(value = DashboardView.ROUTE, layout = MainLayout.class)
@RouteAlias(value = "", layout = MainLayout.class)
@Authorize({Permission.DASHBOARD, Permission.SALES_DASHBOARD})
@Slf4j
public class DashboardView extends AppView {

    public static final String ROUTE = "dashboard";

    private final ProjectService projectService;
    private final InvoicingService invoicingService;
    private final HolidayService holidayService;
    private final ListService listService;
    private final TaskService taskService;
    private final AzureGraphService azureGraphService;
    private final UserService userService;
    private final WorkReportComponentOperation workReportComponentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final SalesDashboardComponentOperation salesDashboardComponentOperation;
    private final ManagementComponentOperation managementComponentOperation;

    public DashboardView(ProjectService projectService, InvoicingService invoicingService,
                         HolidayService holidayService, ListService listService, TaskService taskService,
                         AzureGraphService azureGraphService, UserService userService,
                         WorkReportComponentOperation workReportComponentOperation,
                         EntityNewComponentOperation entityNewComponentOperation,
                         SalesDashboardComponentOperation salesDashboardComponentOperation,
                         ManagementComponentOperation managementComponentOperation) {
        this.projectService = projectService;
        this.invoicingService = invoicingService;
        this.holidayService = holidayService;
        this.listService = listService;
        this.taskService = taskService;
        this.azureGraphService = azureGraphService;
        this.userService = userService;
        this.workReportComponentOperation = workReportComponentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.salesDashboardComponentOperation = salesDashboardComponentOperation;
        this.managementComponentOperation = managementComponentOperation;
        initTab();
    }

    private void initTab() {

        List<TabEntry> tabEntryList = new ArrayList<>();

        if (SecurityUtils.hasPermission(Permission.DASHBOARD)) {
            tabEntryList.add(new TabEntry(Transl.get("Dashboard"), new DashboardTab(projectService, invoicingService,
                    holidayService, listService, taskService, azureGraphService, userService,
                    workReportComponentOperation)));
        }

        if (SecurityUtils.hasPermission(Permission.SALES_DASHBOARD)) {
            tabEntryList.add(new TabEntry(Transl.get("Sales dashboard"), new SalesDashboardTab(
                    salesDashboardComponentOperation, managementComponentOperation)));
        }

        DashboardTabsComponent dashboardTabsComponent = new DashboardTabsComponent("Dashboard", tabEntryList, entityNewComponentOperation);
        dashboardTabsComponent.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation));
        this.add(dashboardTabsComponent);
    }


}
