package cz.bbn.cerberus.taskfollowing.ui.component;

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
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.task.dto.TaskDto;
import cz.bbn.cerberus.taskfollowing.dto.TaskFollowingDto;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

public class TaskFollowingGridComponent extends AppInfiniteGrid<TaskFollowingDto> {

    public TaskFollowingGridComponent(DeleteAction deleteAction, AppEnv appEnv,
                                      ItemsAction<TaskFollowingDto> itemsAction) {
        super(deleteAction, appEnv, itemsAction);
        initGrid();
    }

    private void initGrid() {
        addColumn(taskFollowingDto -> taskFollowingDto.getFollowingUserDto().getName());
        addColumn(new ComponentRenderer<>(this::getValueColumn)).setHeader(Transl.get("Actual tasks"))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);
        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        setItemDetailsRenderer(createTaskFollowingGridDetailsRenderer());
        setDetailsVisibleOnClick(false);
        addItemClickListener(event -> this.setDetailsVisible(event.getItem(), !this.isDetailsVisible(event.getItem())));
    }

    private static ComponentRenderer<
            TaskFollowingGridRowDetail, TaskFollowingDto
            > createTaskFollowingGridDetailsRenderer() {
        return new ComponentRenderer<>(TaskFollowingGridRowDetail::new,
                TaskFollowingGridRowDetail::setRow);
    }

    private HorizontalLayout getGridButtons(TaskFollowingDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");
        if (SecurityUtils.hasPermission(Permission.TASK_FOLLOWING_DELETE)) {
            Button delete = VaadinComponents.getDeleteButton();
            AppUtils.addRfClassToGridButton(delete, String.valueOf(clickedItem.getFollowingUserDto().getId()));
            delete.addClickListener(buttonClickEvent -> {
                DeleteConfirmDialog deleteConfirmDialog =
                        new DeleteConfirmDialog(this, String.valueOf(clickedItem.getFollowingUserDto().getId()),
                                Transl.get("Are you sure you want to delete following for user {0} ?",
                                        String.valueOf(clickedItem.getFollowingUserDto().getName())),
                                getAppEnv(), true);
                deleteConfirmDialog.open();
            });
            delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete task"));
            buttons.add(delete);
        }
        return buttons;
    }

    private HorizontalLayout getValueColumn(TaskFollowingDto item) {
        List<String> taskList = new ArrayList<>();
        for (TaskDto taskDto : item.getTaskDtoList()) {
            taskList.add(taskDto.getName());
        }
        HorizontalLayout horizontalLayout = new HorizontalLayout();
        horizontalLayout.addClassName("buttons-layout");
        horizontalLayout.addClassName("cursor-pointer");
        horizontalLayout.add(getTaskCount(taskList));
        horizontalLayout.getElement().setProperty(TextValues.TITLE, StringUtils.join(taskList, ", "));
        return horizontalLayout;
    }

    private static Span getTaskCount(List<String> taskList) {
        Span taskCountSpan = new Span(String.valueOf((long) taskList.size()));
        taskCountSpan.getElement().getStyle().set("overflow", "hidden").set("white-space", "nowrap")
                .set("text-overflow", "ellipsis");
        return taskCountSpan;
    }

}
