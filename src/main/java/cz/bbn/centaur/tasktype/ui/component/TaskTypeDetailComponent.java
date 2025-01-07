package cz.bbn.cerberus.tasktype.ui.component;

import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.TaskTypeTabComponent;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.appcard.AppDetailCardComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.tasktype.dto.TaskTypeDto;
import cz.bbn.cerberus.tasktype.ui.component.tab.TaskTypeDetailTabComponent;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class TaskTypeDetailComponent extends AppDetailCardComponent<TaskTypeDto>
        implements AppBinderOperation<TaskTypeDto> {

    private final List<RoleDto> roleList;

    public TaskTypeDetailComponent(TaskTypeDto dto, SaveAction<TaskTypeDto> saveAction,
                                   boolean showSubmitButton, AppEnv appEnv,
                                   EntityNewComponentOperation entityNewComponentOperation, List<RoleDto> roleList) {
        super(dto, saveAction, showSubmitButton, appEnv, entityNewComponentOperation);
        this.roleList = roleList;
        initComponent();
    }

    @Override
    protected void initComponent() {
        String heading = getDto().getId() == null ? Transl.get("New task type") :
                Transl.get("Task type")
                        .concat(" - ")
                        .concat(getDto().getName());
        setHeading(heading);
        this.addBackButton(AdministrationView.ROUTE + "/" + TaskTypeTabComponent.TAB_INDEX);
        if (isShowSubmitButton()) {
            this.addSaveButton();
        }
        this.setId(RobotFrameworkVariables.TASK_TYPE_DETAIL_CARD_ID.getValue());
        this.setSizeFull();

        this.add(new TaskTypeDetailTabComponent(this, roleList, false));
    }
}
