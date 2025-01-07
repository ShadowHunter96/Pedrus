package cz.bbn.cerberus.task.dto;

import cz.bbn.cerberus.commons.enums.ObjectType;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;

@Getter
@Setter
public class TaskFilterDto {
    private Long id;
    private String search;
    private Set<TaskState> stateSet;
    private Long assignee;

    private LocalDateTime dateFrom;
    private LocalDateTime dateTo;
    private Boolean showMeSolver;
    private Boolean showAssignedToMe;
    private Long owner;

    private ObjectType objectType;
    private String objectId;
    private String subjectId;

    private Set<Long> assignedUsers;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
