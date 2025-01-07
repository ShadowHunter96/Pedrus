package cz.bbn.cerberus.task.ui.component;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.html.H3;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.task.dto.TaskCheckDto;
import cz.bbn.cerberus.task.dto.TaskDto;
import cz.bbn.cerberus.translation.Transl;

import java.time.LocalDateTime;
import java.util.List;

public class TaskChecklistComponent extends VerticalLayout {

    private final VerticalLayout verticalLayout = new VerticalLayout();

    private List<TaskCheckDto> taskCheckDtoList;

    public TaskChecklistComponent(List<TaskCheckDto> taskCheckDtoList, TaskDto taskDto) {
        initComponent(taskCheckDtoList, taskDto);
    }

    private void initComponent(List<TaskCheckDto> taskCheckDtoList, TaskDto taskDto) {

        setMargin(false);
        setPadding(false);

        add(new H3(Transl.get("Checkbox list")));
        add(verticalLayout);
        //LayoutUtils.setBigInfiniteColumnResponsiveSteps(formLayout);
        generateComponent(taskCheckDtoList, taskDto);
    }

    public void generateComponent(List<TaskCheckDto> taskCheckDtoList, TaskDto taskDto) {
        this.taskCheckDtoList = taskCheckDtoList;
        verticalLayout.removeAll();
        this.setVisible(!taskCheckDtoList.isEmpty());
        if (!taskCheckDtoList.isEmpty()) {
            for (TaskCheckDto taskCheckDto : taskCheckDtoList) {
                Checkbox checkbox = new Checkbox(generateCheckboxName(taskCheckDto, taskDto));
                checkbox.setValue(Boolean.TRUE.equals(taskCheckDto.getValue()));
                checkbox.addValueChangeListener(e -> changeCheckboxValue(e.getValue(), taskCheckDto));
                verticalLayout.add(checkbox);
            }
        }
    }

    private void changeCheckboxValue(Boolean value, TaskCheckDto taskCheckDto) {
        if (Boolean.TRUE.equals(value)) {
            taskCheckDto.setUser(SecurityUtils.getCurrentUserDto());
            taskCheckDto.setCompleteDate(LocalDateTime.now());
        }
        taskCheckDto.setValue(value);
    }

    private String generateCheckboxName(TaskCheckDto taskCheckDto, TaskDto taskDto) {
        StringBuilder name = new StringBuilder();
        name.append(taskCheckDto.getCheckboxName());
        if (taskCheckDto.getDaysToFinish() != null && taskDto.getDate() != null) {
            name.append(" - ").append(Transl.get("Target date")).append(": ").append(
                    AppUtils.formatDateTime(taskDto.getDate().minusDays(taskCheckDto.getDaysToFinish()), true));
        }
        if (Boolean.TRUE.equals(taskCheckDto.getValue())) {
            if (taskCheckDto.getUser() != null) {
                if (taskCheckDto.getUser().getAcronym() != null
                        && !taskCheckDto.getUser().getAcronym().trim().isEmpty()) {
                    name.append(" - ").append(taskCheckDto.getUser().getAcronym());
                } else {
                    name.append(" - ").append(taskCheckDto.getUser().getName());
                }
            }
            if (taskCheckDto.getCompleteDate() != null) {
                name.append(" ").append(AppUtils.formatDateTime(taskCheckDto.getCompleteDate(), true));
            }
        }
        return name.toString();
    }

    public List<TaskCheckDto> getTaskCheckDtoList() {
        return taskCheckDtoList;
    }
}
