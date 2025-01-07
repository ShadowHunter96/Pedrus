package cz.bbn.cerberus.task.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.slidetab.SlideBarCountUpdateAction;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.task.TaskComponentOperation;
import cz.bbn.cerberus.task.dto.TaskCheckDto;
import cz.bbn.cerberus.task.dto.TaskDto;
import cz.bbn.cerberus.task.dto.TaskEntityType;
import cz.bbn.cerberus.tasktemplate.TaskTemplateComponentOperation;
import cz.bbn.cerberus.tasktemplate.dto.TaskTemplateDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.ArrayList;
import java.util.List;

public class TaskPickTemplateDialog extends AppDialog {

    private final TaskComponentOperation componentOperation;
    private final TaskTemplateComponentOperation templateComponentOperation;
    private final AppInfiniteGrid<?> grid;
    private final SlideBarCountUpdateAction getCountUpdateAction;

    private final List<TaskTemplateDto> templateList;

    public TaskPickTemplateDialog(TaskComponentOperation componentOperation,
                                  TaskTemplateComponentOperation templateComponentOperation, AppInfiniteGrid<?> grid,
                                  SlideBarCountUpdateAction getCountUpdateAction) {
        this.componentOperation = componentOperation;
        this.templateComponentOperation = templateComponentOperation;
        this.grid = grid;
        this.getCountUpdateAction = getCountUpdateAction;
        this.templateList = templateComponentOperation.getAllowedTemplateList();
        initComponent();
    }

    private void initComponent() {
        this.setTitle(Transl.get("Pick template"));

        TaskTemplateDto emptyDto = new TaskTemplateDto();
        emptyDto.setName(TextValues.EMPTY_VALUE);
        templateList.add(0, emptyDto);

        ComboBox<TaskTemplateDto> comboBox = new ComboBox<>(Transl.get("Template"));
        comboBox.setItems(templateList);
        comboBox.setItemLabelGenerator(TaskTemplateDto::getName);
        comboBox.setValue(emptyDto);
        setContent(comboBox);

        addCloseButton();

        Button submit = VaadinComponents.getSubmitButton();
        submit.setText(Transl.get("Choose"));
        submit.addClickListener(e -> {
            this.close();
            openEditDialog(comboBox.getValue());
        });
        addSubmitButton(submit);
    }


    private void openEditDialog(TaskTemplateDto dto) {
        TaskDto taskDto = new TaskDto();
        if (dto != null && dto.getId() != null) {
            taskDto = templateComponentOperation.getTaskFromTemplate(dto);
            taskDto.setId(null);
            taskDto.setTaskCheckDtoList(
                    getProcessedCheckList(templateComponentOperation.getTaskCheckList(dto.getId())));
            taskDto.setUserDto(SecurityUtils.getCurrentUserDto());
        }
        TaskEditDialog taskEditDialog = new TaskEditDialog(
                taskDto, grid, componentOperation, getCountUpdateAction, TaskEntityType.TASK);
        taskEditDialog.open();
    }

    private List<TaskCheckDto> getProcessedCheckList(List<TaskCheckDto> taskCheckList) {
        List<TaskCheckDto> toReturnList = new ArrayList<>();
        for (TaskCheckDto checkDto : taskCheckList) {
            checkDto.setId(null);
            toReturnList.add(checkDto);
        }
        return toReturnList;
    }
}
