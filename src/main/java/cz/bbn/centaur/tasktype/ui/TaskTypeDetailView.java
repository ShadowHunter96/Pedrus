package cz.bbn.cerberus.tasktype.ui;

import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.tasktype.TaskTypeComponentOperation;
import cz.bbn.cerberus.tasktype.dto.TaskTypeDto;
import cz.bbn.cerberus.tasktype.ui.component.TaskTypeDetailComponent;
import lombok.extern.slf4j.Slf4j;

@Route(value = TaskTypeDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.TASK_TYPE_VIEW)
@Slf4j
public class TaskTypeDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "task-detail";

    private final cz.bbn.cerberus.tasktype.TaskTypeComponentOperation taskTypeComponentOperation;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public TaskTypeDetailView(TaskTypeComponentOperation taskTypeComponentOperation,
                              AppEnv appEnv, EntityNewComponentOperation entityNewComponentOperation) {
        this.taskTypeComponentOperation = taskTypeComponentOperation;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(TaskTypeDto dto) {
        TaskTypeDetailComponent taskTypeDetailComponent =
                new TaskTypeDetailComponent(dto, taskTypeComponentOperation.getSaveAction(null),
                        SecurityUtils.hasPermission(Permission.TASK_TYPE_EDIT),
                        appEnv, entityNewComponentOperation, taskTypeComponentOperation.getRoleList());
        this.add(taskTypeDetailComponent);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, String param) {
        try {
            TaskTypeDto dto = taskTypeComponentOperation.getTaskType(param);
            refreshBreadcrumbText(String.valueOf(dto.getId()));
            initView(dto);
        } catch (SystemException ex) {
            log.error(TextValues.SYSTEM_EXCEPTION, ex);
            ErrorNotification.show(ex, appEnv);
        }
    }
}
