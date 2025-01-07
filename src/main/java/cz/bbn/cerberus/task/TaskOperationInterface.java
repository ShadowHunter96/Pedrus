package cz.bbn.cerberus.task;

import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.task.dto.TaskCheckDto;
import cz.bbn.cerberus.task.dto.TaskDto;
import cz.bbn.cerberus.tasktype.dto.TaskTypeDto;
import cz.bbn.cerberus.user.dto.UserDto;

import java.util.List;

public interface TaskOperationInterface {

    List<UserDto> getAllowedUsers();

    List<RoleDto> getAllowedRoles();

    List<ItemDto> loadItemDtoListByObjectType(ObjectType value);

    SaveAction<TaskDto> getSaveAction();

    List<TaskCheckDto> getTaskCheckList(Long id);

    List<TaskTypeDto> getAllowedTaskTypeList(ObjectType objectType);

    List<UserDto> getAssigneeUserList(TaskDto dto);
}
