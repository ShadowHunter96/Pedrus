package cz.bbn.cerberus.task.ui.component;

import cz.bbn.cerberus.task.dto.TaskCheckDto;

import java.util.List;

public interface SetChecklistAction {
    void setChecklist(List<TaskCheckDto> taskCheckDtoList);
}
