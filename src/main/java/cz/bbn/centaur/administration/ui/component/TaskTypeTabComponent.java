package cz.bbn.cerberus.administration.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.tasktype.TaskTypeComponentOperation;
import cz.bbn.cerberus.tasktype.TaskTypeService;
import cz.bbn.cerberus.tasktype.dto.TaskTypeDto;
import cz.bbn.cerberus.tasktype.ui.component.TaskTypeGridComponent;
import cz.bbn.cerberus.tasktype.ui.component.TaskTypeNewDialog;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class TaskTypeTabComponent extends TabSimpleComponent {

    public static final int TAB_INDEX = 10;
    private final TaskTypeService taskTypeService;
    private final TaskTypeComponentOperation taskTypeComponentOperation;
    private final AppEnv appEnv;

    private TaskTypeGridComponent taskTypeGridComponent;

    public TaskTypeTabComponent(AppEnv appEnv, TaskTypeService taskTypeService,
                                TaskTypeComponentOperation taskTypeComponentOperation) {
        this.taskTypeComponentOperation = taskTypeComponentOperation;
        this.taskTypeService = taskTypeService;
        this.appEnv = appEnv;
        initComponent();
    }

    private void initComponent() {
        setSizeFull();
        setId(RobotFrameworkVariables.TASK_TYPE_TAB_CARD_ID.getValue());
        taskTypeGridComponent = new TaskTypeGridComponent(getDeleteAction(), appEnv, getItemsAction());

        taskTypeGridComponent.loadData();
        this.add(taskTypeGridComponent);

    }

    @Override
    public Button getFooterButton() {
        if (SecurityUtils.hasPermission(Permission.TASK_TYPE_EDIT)) {
            Button addNew = VaadinComponents.getNewButton(Transl.get("Add task type"));
            addNew.addClickListener(e -> new TaskTypeNewDialog(
                    taskTypeGridComponent, taskTypeComponentOperation).open());
            return addNew;
        }
        return null;
    }

    private DeleteAction getDeleteAction() {
        return id -> {
            try {
                taskTypeService.deleteTaskType(Long.parseLong(id));
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    private ItemsAction<TaskTypeDto> getItemsAction() {
        return (query, orderList) ->
                taskTypeService.findTaskTypeDtoPage(query.getPage(), query.getPageSize(), orderList);
    }

}
