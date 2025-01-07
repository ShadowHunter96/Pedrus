package cz.bbn.cerberus.tasktype.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.tasktype.TaskTypeComponentOperation;
import cz.bbn.cerberus.tasktype.dto.TaskTypeDto;
import cz.bbn.cerberus.tasktype.ui.component.tab.TaskTypeDetailTabComponent;
import cz.bbn.cerberus.translation.Transl;

public class TaskTypeNewDialog extends AppDialog implements AppBinderOperation<TaskTypeDto> {

    private final AppInfiniteGrid<TaskTypeDto> grid;
    private final TaskTypeComponentOperation taskTypeComponentOperation;

    private final Binder<TaskTypeDto> binder = new Binder<>();
    private final TaskTypeDto dto = new TaskTypeDto();

    public TaskTypeNewDialog(AppInfiniteGrid<TaskTypeDto> grid,
                             TaskTypeComponentOperation taskTypeComponentOperation) {
        this.grid = grid;
        this.taskTypeComponentOperation = taskTypeComponentOperation;
        init();
    }

    void init() {
        setTitle(Transl.get("New task type"));

        TaskTypeDetailTabComponent taskTypeDetailTabComponent =
                new TaskTypeDetailTabComponent(this, taskTypeComponentOperation.getRoleList(), true);
        setContent(taskTypeDetailTabComponent);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            if (binder.validate().isOk()) {
                taskTypeComponentOperation.getSaveAction(this).saveItem(dto, null);
                if (grid != null) {
                    grid.loadData();
                }
            } else {
                ErrorNotification.show(Transl.get(TextValues.INVALIS_INPUT),
                        taskTypeComponentOperation.getAppEnv());
            }
            submit.setEnabled(true);
        });

        showWarning(true);

        addCloseButton();
        addButtons(submit);
    }

    @Override
    public Binder<TaskTypeDto> getBinder() {
        return binder;
    }

    @Override
    public TaskTypeDto getDto() {
        return dto;
    }
}
