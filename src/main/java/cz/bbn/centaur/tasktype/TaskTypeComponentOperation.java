package cz.bbn.cerberus.tasktype;

import com.vaadin.flow.component.UI;
import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.TaskTypeTabComponent;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.role.RoleService;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.tasktype.dto.TaskTypeDto;
import cz.bbn.cerberus.tasktype.ui.TaskTypeDetailView;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class TaskTypeComponentOperation {

    private final TaskTypeService taskTypeService;
    private final RoleService roleService;
    private final AppEnv appEnv;

    public TaskTypeComponentOperation(TaskTypeService taskTypeService, RoleService roleService, AppEnv appEnv) {
        this.taskTypeService = taskTypeService;
        this.roleService = roleService;
        this.appEnv = appEnv;
    }

    public SaveAction<TaskTypeDto> getSaveAction(AppDialog appDialog) {
        return (newDto, originalDto) -> {
            try {
                if (originalDto != null) {
                    taskTypeService.updateTaskType(newDto, originalDto);
                    UI.getCurrent().navigate(AdministrationView.ROUTE.concat("/")
                            .concat(String.valueOf(TaskTypeTabComponent.TAB_INDEX)));

                } else {
                    Long id = taskTypeService.saveTaskType(newDto);
                    if (appDialog != null) {
                        appDialog.showWarning(false);
                        appDialog.close();
                        UI.getCurrent().navigate(
                                TaskTypeDetailView.ROUTE.concat("/").concat(String.valueOf(id)));
                    }
                }
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public AppEnv getAppEnv() {
        return appEnv;
    }

    public List<RoleDto> getRoleList() {
        return roleService.findAll();
    }

    public TaskTypeDto getTaskType(String param) throws SystemException {
        return taskTypeService.getTaskType(Long.valueOf(param));
    }
}
