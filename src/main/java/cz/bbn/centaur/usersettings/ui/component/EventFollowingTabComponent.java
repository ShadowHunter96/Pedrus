package cz.bbn.cerberus.usersettings.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.taskfollowing.TaskFollowingComponentOperation;
import cz.bbn.cerberus.taskfollowing.ui.component.TaskFollowingEditDialog;
import cz.bbn.cerberus.taskfollowing.ui.component.TaskFollowingFilterComponent;
import cz.bbn.cerberus.taskfollowing.ui.component.TaskFollowingGridComponent;
import cz.bbn.cerberus.translation.Transl;

public class EventFollowingTabComponent extends TabSimpleComponent {

    public static final int TAB_INDEX = 0;

    private final TaskFollowingComponentOperation taskFollowingComponentOperation;
    private final AppEnv appEnv;

    private TaskFollowingGridComponent taskFollowingGridComponent;

    public EventFollowingTabComponent(TaskFollowingComponentOperation taskFollowingComponentOperation,
                                      AppEnv appEnv) {
        this.taskFollowingComponentOperation = taskFollowingComponentOperation;
        this.appEnv = appEnv;
        initComponent();
    }

    private void initComponent() {
        setSizeFull();
        setId(RobotFrameworkVariables.TASK_FOLLOWING_TAB_CARD_ID.getValue());

        Button search = VaadinComponents.getSearchButton();
        TaskFollowingFilterComponent taskFollowingFilterComponent = new TaskFollowingFilterComponent(search);
        this.add(taskFollowingFilterComponent);

        taskFollowingGridComponent =
                new TaskFollowingGridComponent(taskFollowingComponentOperation.getDeleteAction(), appEnv,
                        taskFollowingComponentOperation.getItemsAction(taskFollowingFilterComponent));

        taskFollowingGridComponent.loadData();
        search.addClickListener(buttonClickEvent -> taskFollowingGridComponent.loadData());
        this.add(taskFollowingFilterComponent);
        this.add(taskFollowingGridComponent);
    }

    @Override
    public Button getFooterButton() {
        if (SecurityUtils.hasPermission(Permission.TASK_FOLLOWING_EDIT)) {
            Button addNew = VaadinComponents.getNewButton(Transl.get("Edit event follow"));
            addNew.addClickListener(e ->
                    new TaskFollowingEditDialog(taskFollowingComponentOperation, taskFollowingGridComponent).open());
            return addNew;
        }
        return null;
    }
}
