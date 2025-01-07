package cz.bbn.cerberus.taskfollowing.ui.component;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.taskfollowing.TaskFollowingComponentOperation;
import cz.bbn.cerberus.taskfollowing.dto.TaskFollowingDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.Set;

public class TaskFollowingUserGridComponent extends Grid<TaskFollowingDto> {

    private final Set<TaskFollowingDto> taskFollowingDtoSet;
    private final TaskFollowingComponentOperation taskFollowingComponentOperation;

    public TaskFollowingUserGridComponent(Set<TaskFollowingDto> taskFollowingDtoSet,
                                          TaskFollowingComponentOperation taskFollowingComponentOperation) {
        this.taskFollowingDtoSet = taskFollowingDtoSet;
        this.taskFollowingComponentOperation = taskFollowingComponentOperation;
        initGrid();
    }

    void initGrid() {
        addColumn(taskFollowingDto ->
                taskFollowingDto.getFollowingUserDto().getName()).setHeader(Transl.get("User")).setSortable(true);
        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Follow")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);
        setWidth("30em");
    }

    public void loadData(TaskFollowingFilterComponent taskFollowingFilterComponent) {
        setItems(taskFollowingComponentOperation.getFollowingDtoAllSet(taskFollowingFilterComponent));
    }

    private HorizontalLayout getGridButtons(TaskFollowingDto clickedItem) {
        HorizontalLayout layout = new HorizontalLayout();
        layout.setClassName("buttons-layout");
        Checkbox checkbox = new Checkbox();
        checkbox.setValue(taskFollowingDtoSet.contains(clickedItem));
        checkbox.addValueChangeListener(event -> {
            if (Boolean.TRUE.equals(event.getValue())) {
                taskFollowingDtoSet.add(clickedItem);
            } else {
                taskFollowingDtoSet.remove(clickedItem);
            }
        });
        layout.add(checkbox);
        return layout;
    }
}
