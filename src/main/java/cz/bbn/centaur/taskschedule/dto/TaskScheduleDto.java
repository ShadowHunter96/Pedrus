package cz.bbn.cerberus.taskschedule.dto;

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

import java.io.Serializable;
import java.time.LocalDate;
import java.util.List;
import java.util.Set;

@Getter
@Setter
public class TaskScheduleDto implements Serializable {

    private Long id;
    private String name;
    private String description;
    private ObjectType objectType;
    private String objectId;
    private String subjectId;
    private ItemDto itemDto;
    private TaskState state;
    private Integer daysToNotify;
    private NotifyFrequency notifyFrequency;
    private TaskColor color;
    private UserDto assignee;
    private UserDto userDto;
    private Set<SendTask> sendTaskSet;
    private Set<UserDto> userSet;
    private Set<RoleDto> roleSet;
    private List<TaskCheckDto> taskCheckList;
    private boolean deleted;
    private boolean sendToOutlook;
    private TaskScheduleFrequency frequency;
    private Integer creationDay;
    private LocalDate creationDate;
    private TaskTypeDto taskType;

}
