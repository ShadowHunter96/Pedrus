package cz.bbn.cerberus.applog.ui;

import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.applog.ui.components.tab.AppLogTab;
import cz.bbn.cerberus.applog.ui.components.tab.SchedulerLogTab;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.mainlayout.ui.Navigation;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.schedulerlog.SchedulerLogService;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;

import java.util.ArrayList;
import java.util.List;

@Route(value = AppLogView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.APP_LOG_VIEW)
public class AppLogView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "app-log";

    private final AppEnv appEnv;
    private final AppLogService appLogService;
    private final SchedulerLogService schedulerLogService;
    private final UserService userService;
    private final Navigation navigation;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public AppLogView(AppEnv appEnv, AppLogService appLogService,
                      SchedulerLogService schedulerLogService, UserService userService, Navigation navigation,
                      EntityNewComponentOperation entityNewComponentOperation) {
        this.appEnv = appEnv;
        this.appLogService = appLogService;
        this.schedulerLogService = schedulerLogService;
        this.userService = userService;
        this.navigation = navigation;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(Integer activeTab) {
        removeAll();
        setSizeFull();
        List<TabEntry> tabEntryList = new ArrayList<>();
        tabEntryList.add(new TabEntry(Transl.get("App log"),
                new AppLogTab(appEnv, appLogService, userService), Permission.APP_LOG_VIEW, AppLogTab.TAB_INDEX));
        tabEntryList.add(new TabEntry(Transl.get("Scheduler log"),
                new SchedulerLogTab(appEnv, schedulerLogService),
                Permission.SCHEDULER_LOG_VIEW,
                SchedulerLogTab.TAB_INDEX));

        String heading = Transl.get("Logs");

        AppLogTabs appLogTabsComponent = new AppLogTabs(
                heading, tabEntryList, activeTab, navigation, entityNewComponentOperation);
        appLogTabsComponent.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation));
        add(appLogTabsComponent);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String activeTab) {
        initView(activeTab == null ? 0 : Integer.parseInt(activeTab));
    }

}
