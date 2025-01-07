package cz.bbn.cerberus.taskfollowing.ui.component;

import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.provider.ListDataProvider;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.task.dto.TaskDto;
import cz.bbn.cerberus.taskfollowing.dto.TaskFollowingDto;
import cz.bbn.cerberus.translation.Transl;


public class TaskFollowingGridRowDetail extends HorizontalLayout {

    public void setRow(TaskFollowingDto taskFollowingDto) {
        this.removeAll();
        this.setHeight("15em");
        this.setAlignItems(Alignment.CENTER);
        Grid<TaskDto> grid = new Grid<>();
        grid.setWidthFull();
        grid.setHeightFull();
        grid.addColumn(TaskDto::getName).setHeader(Transl.get("Task"));
        grid.addColumn(taskDto -> AppUtils.formatDateTime(taskDto.getDate(), true)).setHeader(Transl.get("Date"));
        ListDataProvider<TaskDto> dataProvider = new ListDataProvider<>(taskFollowingDto.getTaskDtoList());
        grid.setDataProvider(dataProvider);
        this.add(grid);
    }
}
