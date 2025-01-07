package cz.bbn.cerberus.taskschedule.dto;

import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.tasktype.dto.TaskTypeDto;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;
import java.util.Set;

@Getter
@Setter
public class TaskScheduleFilterDto {

    private Long id;
    private String search;

    private Long owner;

    private boolean showMine;
    private boolean showDeleted;

    private ObjectType objectType;
    private String objectId;
    private TaskTypeDto taskType;

    private Set<UserDto> assignedUserSet;
    private TaskScheduleFrequency frequency;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
