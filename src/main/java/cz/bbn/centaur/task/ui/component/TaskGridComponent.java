package cz.bbn.cerberus.task.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.html.Div;
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
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.task.TaskComponentOperation;
import cz.bbn.cerberus.task.dto.TaskDto;
import cz.bbn.cerberus.task.dto.TaskEntityType;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserActiveRoleDto;
import cz.bbn.cerberus.user.dto.UserDto;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

public class TaskGridComponent extends AppInfiniteGrid<TaskDto> {

    private final TaskComponentOperation taskComponentOperation;

    public TaskGridComponent(AppEnv appEnv,
                             TaskFilterComponent taskFilterComponent,
                             TaskComponentOperation taskComponentOperation) {
        super(taskComponentOperation.getDeleteAction(), appEnv,
                taskComponentOperation.getItemsAction(taskFilterComponent));
        this.taskComponentOperation = taskComponentOperation;
        initGrid();
    }

    private void initGrid() {
        addColumn(new ComponentRenderer<>(this::getColor)).setHeader(Transl.get("Priority"))
                .setSortable(true).setKey("color");
        addColumn(new ComponentRenderer<>(this::getCreationDate)).setHeader(Transl.get("Created"))
                .setSortable(true).setKey("creationDate");
        addColumn(taskDto -> AppUtils.formatDateTime(taskDto.getDate(), true))
                .setHeader(Transl.get("Target date"))
                .setSortable(true)
                .setKey("date");
        addColumn(TaskDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        addColumn(new ComponentRenderer<>(this::getState)).setHeader(Transl.get("State"))
                .setSortable(true).setKey("state");
        addColumn(new ComponentRenderer<>(this::getUserName)).setHeader(Transl.get("Created by"))
                .setSortable(true).setKey("userEntity");
        addColumn(new ComponentRenderer<>(this::getAssignee)).setHeader(Transl.get("Solver"))
                .setSortable(true).setKey("assignee");
        setColumnReorderingAllowed(true);
        setMultiSort(true);

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(task -> openEditDialog(task.getItem().getId()));

        this.setClassNameGenerator(this::getAssigned);

    }

    private String getAssigned(TaskDto e) {
        if (e.getUserDtoSet() != null) {
            for (UserDto userDto : e.getUserDtoSet()) {
                if (Objects.equals(userDto.getId(), SecurityUtils.getCurrentUserId())) {
                    return "mine-grid-item";
                }
            }
        }
        if (e.getRoleDtoSet() != null) {
            Set<UserActiveRoleDto> roleSet = SecurityUtils.getCurrentUserDto().getUserActiveRoleDtoSet();
            Set<String> stringRoleSet = new HashSet<>();
            for (UserActiveRoleDto roleDto : roleSet) {
                stringRoleSet.add(roleDto.getRoleId());
            }
            for (RoleDto roleDto : e.getRoleDtoSet()) {
                if (stringRoleSet.contains(roleDto.getId())) {
                    return "mine-grid-item";
                }
            }
        }
        return null;
    }

    private Span getUserName(TaskDto dto) {
        if (dto.getUserDto() != null) {
            if (dto.getUserDto().getAcronym() != null && !dto.getUserDto().getAcronym().isEmpty()) {
                Span assignee = new Span(dto.getUserDto().getAcronym());
                assignee.getElement().setProperty("title", dto.getUserDto().getName());
                return assignee;
            } else {
                return new Span(dto.getUserDto().getName());
            }
        }
        return new Span();
    }

    private HorizontalLayout getGridButtons(TaskDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");

        if (SecurityUtils.hasPermission(Permission.TASK_DELETE)) {
            Button delete = VaadinComponents.getDeleteButton();
            AppUtils.addRfClassToGridButton(delete, String.valueOf(clickedItem.getId()));
            delete.addClickListener(buttonClickEvent -> {
                DeleteConfirmDialog deleteConfirmDialog =
                        new DeleteConfirmDialog(this, String.valueOf(clickedItem.getId()),
                                Transl.get("Are you sure you want to delete task {0} ?",
                                        String.valueOf(clickedItem.getId())), getAppEnv(), true);
                deleteConfirmDialog.open();
            });
            delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete task"));
            buttons.add(delete);
        }
        return buttons;
    }

    private void openEditDialog(Long id) {
        TaskDto taskDto = taskComponentOperation.getTaskDto(id);
        TaskEditDialog taskEditDialog = new TaskEditDialog(
                taskDto, this, taskComponentOperation, null, TaskEntityType.TASK);
        taskEditDialog.open();
    }

    private Div getColor(TaskDto taskDto) {
        if (taskDto.getColor() != null) {
            Div colorSpan = new Div();
            colorSpan.setWidth("1.5em");
            colorSpan.setHeight("1.5em");
            colorSpan.getElement().getStyle().set("background-color", taskDto.getColor().getColor())
                    .set("border-radius", "0.3em");
            return colorSpan;
        }
        return new Div();
    }

    private Span getCreationDate(TaskDto taskDto) {
        if (taskDto.getCreationDate() != null) {
            return new Span(AppUtils.formatDateTime(taskDto.getCreationDate(), true));
        }
        return new Span();
    }

    private Span getAssignee(TaskDto taskDto) {
        if (taskDto.getAssignee() != null) {
            if (taskDto.getAssignee().getAcronym() != null && !taskDto.getAssignee().getAcronym().trim().isEmpty()) {
                Span assignee = new Span(taskDto.getAssignee().getAcronym());
                assignee.getElement().setProperty("title", taskDto.getAssignee().getName());
                return assignee;
            } else {
                return new Span(taskDto.getAssignee().getName());
            }
        }
        return new Span();
    }

    private Span getState(TaskDto taskDto) {
        if (taskDto.getState() != null) {
            Span stateSpan = new Span();
            stateSpan.add(taskDto.getState().getIconName().create());
            stateSpan.getElement().setProperty("title", taskDto.getState().getTranslatedValue());
            return stateSpan;
        }
        return new Span();
    }

}
