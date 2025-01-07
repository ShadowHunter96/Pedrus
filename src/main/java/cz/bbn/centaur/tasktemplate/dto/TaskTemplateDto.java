package cz.bbn.cerberus.tasktemplate.dto;

import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.task.dto.NotifyFrequency;
import cz.bbn.cerberus.task.dto.SendTask;
import cz.bbn.cerberus.task.dto.TaskCheckDto;
import cz.bbn.cerberus.task.dto.TaskColor;
import cz.bbn.cerberus.task.dto.TaskState;
import cz.bbn.cerberus.tasktype.dto.TaskTypeDto;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.Set;

@Getter
@Setter
public class TaskTemplateDto {

    private Long id;

    private String name;
    private String description;

    private ObjectType objectType;
    private String objectId;
    private ItemDto itemDto;
    private String subjectId;
    private UserDto userDto;
    private TaskState state;
    private Integer daysToNotify;
    private NotifyFrequency notifyFrequency;
    private TaskColor color;
    private UserDto assignee;
    private RoleDto allowedRole;
    private TaskTypeDto taskType;

    private Set<SendTask> sendTaskSet;

    private Set<UserDto> userDtoSet;
    private Set<RoleDto> roleDtoSet;

    private List<TaskCheckDto> taskCheckDtoList;

    private boolean deleted;

    private boolean sendToOutlook;
}