package cz.bbn.cerberus.usersettings.ui;

import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.taskfollowing.TaskFollowingComponentOperation;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.usersettings.ui.component.EventFollowingTabComponent;
import cz.bbn.cerberus.usersettings.ui.component.UserSettingsTabsComponent;

import java.util.ArrayList;
import java.util.List;


@Route(value = UserSettingsView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.USER_SETTINGS_VIEW)
public class UserSettingsView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "user-settings";

    private final TaskFollowingComponentOperation taskFollowingComponentOperation;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public UserSettingsView(TaskFollowingComponentOperation taskFollowingComponentOperation, AppEnv appEnv,
                            EntityNewComponentOperation entityNewComponentOperation) {
        this.taskFollowingComponentOperation = taskFollowingComponentOperation;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(int activeTab) {
        removeAll();
        setSizeFull();

        List<TabEntry> tabList = new ArrayList<>();

        tabList.add(new TabEntry(Transl.get("Event following"),
                new EventFollowingTabComponent(taskFollowingComponentOperation, appEnv),
                Permission.TASK_FOLLOWING_VIEW, EventFollowingTabComponent.TAB_INDEX));

        UserSettingsTabsComponent userSettingsTabsComponent = new UserSettingsTabsComponent(
                Transl.get("User settings"), tabList, activeTab, entityNewComponentOperation);
        userSettingsTabsComponent.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation));
        this.add(userSettingsTabsComponent);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String activeTab) {
        initView(activeTab == null ? EventFollowingTabComponent.TAB_INDEX : Integer.parseInt(activeTab));
    }
}
