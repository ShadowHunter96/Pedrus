package cz.bbn.cerberus.taskfollowing.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.taskfollowing.TaskFollowingComponentOperation;
import cz.bbn.cerberus.taskfollowing.dto.TaskFollowingDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.Set;

public class TaskFollowingEditDialog extends AppDialog {

    private final TaskFollowingComponentOperation taskFollowingComponentOperation;
    private final TaskFollowingGridComponent taskFollowingGridComponent;

    public TaskFollowingEditDialog(TaskFollowingComponentOperation taskFollowingComponentOperation,
                                   TaskFollowingGridComponent taskFollowingGridComponent) {
        this.taskFollowingComponentOperation = taskFollowingComponentOperation;
        this.taskFollowingGridComponent = taskFollowingGridComponent;
        initComponent();
    }


    private void initComponent() {
        setTitle(Transl.get("Edit task following"));
        VerticalLayout verticalLayout = new VerticalLayout();

        Button search = VaadinComponents.getSearchButton();
        TaskFollowingFilterComponent taskFollowingFilterComponent = new TaskFollowingFilterComponent(search);
        verticalLayout.add(taskFollowingFilterComponent);

        Set<TaskFollowingDto> taskFollowingDtoSet = taskFollowingComponentOperation.getFollowingDtoSet();
        TaskFollowingUserGridComponent taskFollowingGrid =
                new TaskFollowingUserGridComponent(taskFollowingDtoSet, taskFollowingComponentOperation);
        taskFollowingGrid.loadData(taskFollowingFilterComponent);
        verticalLayout.add(taskFollowingGrid);

        search.addClickListener(buttonClickEvent ->
                taskFollowingGrid.loadData(taskFollowingFilterComponent));
        setContent(verticalLayout);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(buttonClickEvent -> {
                    taskFollowingComponentOperation.saveTaskFollowing(taskFollowingDtoSet);
                    this.taskFollowingGridComponent.loadData();
                    this.close();
                }
        );

        addCloseButton();
        addSubmitButton(submit);
    }
}
