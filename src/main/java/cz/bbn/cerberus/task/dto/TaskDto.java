package cz.bbn.cerberus.task.dto;

import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.taskschedule.dto.TaskScheduleFrequency;
import cz.bbn.cerberus.tasktype.dto.TaskTypeDto;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;

@Getter
@Setter
@ToString
public class TaskDto implements Serializable {

    private Long id;

    private String name;
    private String description;
    private LocalDateTime date;
    private UserDto userDto;
    private boolean sendToOutlook;

    private ObjectType objectType;
    private String objectId;
    private ItemDto itemDto;
    private String subjectId;
    private TaskState state;

    private Integer daysToNotify;
    private NotifyFrequency notifyFrequency;
    private TaskColor color;
    private TaskScheduleFrequency frequency;
    private Integer creationDay;

    private Set<UserDto> userDtoSet;
    private Set<RoleDto> roleDtoSet;

    private Set<SendTask> sendTaskSet;

    private List<TaskCheckDto> taskCheckDtoList;

    private LocalDateTime creationDate;

    private UserDto assignee;

    private RoleDto allowedRole;

    private TaskTypeDto taskType;

    private Boolean deleted;

}
