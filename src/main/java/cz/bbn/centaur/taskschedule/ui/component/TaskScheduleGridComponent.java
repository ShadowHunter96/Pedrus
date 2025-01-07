package cz.bbn.cerberus.taskschedule.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.DeleteConfirmDialog;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.task.dto.TaskDto;
import cz.bbn.cerberus.task.dto.TaskEntityType;
import cz.bbn.cerberus.task.ui.component.TaskEditDialog;
import cz.bbn.cerberus.taskschedule.TaskScheduleComponentOperation;
import cz.bbn.cerberus.taskschedule.dto.TaskScheduleDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

public class TaskScheduleGridComponent extends AppInfiniteGrid<TaskScheduleDto> {

    private final TaskScheduleComponentOperation componentOperation;

    public TaskScheduleGridComponent(AppEnv appEnv, TaskScheduleFilterComponent filterComponent,
                                     TaskScheduleComponentOperation componentOperation) {
        super(componentOperation.getDeleteAction(), appEnv, componentOperation.getItemsAction(filterComponent));
        this.componentOperation = componentOperation;
        initGrid();
    }

    private void initGrid() {
        addColumn(TaskScheduleDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        addColumn(new ComponentRenderer<>(this::getState)).setHeader(Transl.get("State"))
                .setSortable(true).setKey("state");
        addColumn(new ComponentRenderer<>(this::getAssignee)).setHeader(Transl.get("Assigned users"))
                .setKey("assignee");
        addColumn(new ComponentRenderer<>(this::getFrequency)).setHeader(Transl.get("Frequency"))
                .setSortable(true).setKey("frequency");
        addColumn(new ComponentRenderer<>(this::getTaskType)).setHeader(Transl.get("Task type"));
        setColumnReorderingAllowed(true);
        setMultiSort(true);

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(schedule -> openEditDialog(schedule.getItem()));

    }

    private HorizontalLayout getGridButtons(TaskScheduleDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");

        if (SecurityUtils.hasPermission(Permission.TASK_SCHEDULE_DELETE)) {
            Button delete = VaadinComponents.getDeleteButton();
            AppUtils.addRfClassToGridButton(delete, String.valueOf(clickedItem.getId()));
            delete.addClickListener(buttonClickEvent -> {
                DeleteConfirmDialog deleteConfirmDialog =
                        new DeleteConfirmDialog(this, String.valueOf(clickedItem.getId()),
                                Transl.get("Are you sure you want to delete task schedule {0} ?",
                                        String.valueOf(clickedItem.getId())), getAppEnv(), true);
                deleteConfirmDialog.open();
            });
            delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete task schedule"));
            buttons.add(delete);
        }
        return buttons;
    }

    private void openEditDialog(TaskScheduleDto dto) {
        TaskDto taskDto = componentOperation.getTaskFromSchedule(dto);
        TaskEditDialog taskEditDialog = new TaskEditDialog(
                taskDto, this, componentOperation, null, TaskEntityType.SCHEDULE);
        taskEditDialog.open();
    }

    private Span getAssignee(TaskScheduleDto dto) {
        List<String> acronymList = new ArrayList<>();
        List<String> nameList = new ArrayList<>();
        if (dto.getUserSet() != null && !dto.getUserSet().isEmpty()) {
            for (UserDto userDto : dto.getUserSet()) {
                if (userDto.getAcronym() != null && !userDto.getAcronym().trim().isEmpty()) {
                    acronymList.add(userDto.getAcronym());
                } else {
                    acronymList.add(userDto.getName());
                }
                nameList.add(userDto.getName());
            }
        }
        String acronymStr = StringUtils.join(acronymList, ", ");
        String nameStr = StringUtils.join(nameList, ", ");
        Span toReturn = new Span(acronymStr);
        toReturn.getElement().setProperty("title", nameStr);
        return toReturn;
    }

    private Span getState(TaskScheduleDto dto) {
        if (dto.getState() != null) {
            Span stateSpan = new Span();
            stateSpan.add(dto.getState().getIconName().create());
            stateSpan.getElement().setProperty("title", dto.getState().getTranslatedValue());
            return stateSpan;
        }
        return new Span();
    }

    private Span getFrequency(TaskScheduleDto dto) {
        if (dto.getFrequency() != null) {
            return new Span(dto.getFrequency().getTranslatedValue());
        }
        return new Span();
    }

    private Span getTaskType(TaskScheduleDto dto) {
        if (dto.getTaskType() != null) {
            return new Span(dto.getTaskType().getName());
        }
        return new Span();
    }
}
