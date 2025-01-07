package cz.bbn.cerberus.tasktemplate.ui.component;

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
import cz.bbn.cerberus.tasktemplate.TaskTemplateComponentOperation;
import cz.bbn.cerberus.tasktemplate.dto.TaskTemplateDto;
import cz.bbn.cerberus.translation.Transl;

public class TaskTemplateGridComponent extends AppInfiniteGrid<TaskTemplateDto> {

    private final TaskTemplateComponentOperation componentOperation;

    public TaskTemplateGridComponent(AppEnv appEnv, TaskTemplateFilterComponent filterComponent,
                                     TaskTemplateComponentOperation componentOperation) {
        super(componentOperation.getDeleteAction(), appEnv,
                componentOperation.getItemsAction(filterComponent));
        this.componentOperation = componentOperation;
        initGrid();
    }

    private void initGrid() {
        addColumn(new ComponentRenderer<>(this::getUser)).setHeader(Transl.get("Owner"))
                .setSortable(true).setKey("userEntity");
        addColumn(new ComponentRenderer<>(this::getRoles)).setHeader(Transl.get("Roles"));
        addColumn(TaskTemplateDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        setColumnReorderingAllowed(true);
        setMultiSort(true);

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(task -> openEditDialog(task.getItem()));

    }

    private HorizontalLayout getGridButtons(TaskTemplateDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");

        if (SecurityUtils.hasPermission(Permission.TASK_TEMPLATE_DELETE)) {
            Button delete = VaadinComponents.getDeleteButton();
            AppUtils.addRfClassToGridButton(delete, String.valueOf(clickedItem.getId()));
            delete.addClickListener(buttonClickEvent -> {
                DeleteConfirmDialog deleteConfirmDialog =
                        new DeleteConfirmDialog(this, String.valueOf(clickedItem.getId()),
                                Transl.get("Are you sure you want to delete task template {0} ?",
                                        String.valueOf(clickedItem.getId())), getAppEnv(), true);
                deleteConfirmDialog.open();
            });
            delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete task template"));
            buttons.add(delete);
        }
        return buttons;
    }

    private void openEditDialog(TaskTemplateDto dto) {
        TaskDto taskDto = componentOperation.getTaskFromTemplate(dto);
        TaskEditDialog taskEditDialog = new TaskEditDialog(
                taskDto, this, componentOperation, null, TaskEntityType.TEMPLATE);
        taskEditDialog.open();
    }

    private Span getUser(TaskTemplateDto dto) {
        if (dto.getUserDto() != null) {
            return new Span(dto.getUserDto().getName());
        }
        return new Span();
    }

    private Span getRoles(TaskTemplateDto dto) {
        if (dto.getAllowedRole() != null) {
            return new Span(dto.getAllowedRole().getDescription());
        }
        return new Span();
    }
}
