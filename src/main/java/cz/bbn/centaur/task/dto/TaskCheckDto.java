package cz.bbn.cerberus.task.dto;

import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.time.LocalDateTime;

@Getter
@Setter
public class TaskCheckDto implements Serializable {

    private Long id;
    private String entityType;
    private Long entityId;
    private String checkboxName;
    private Boolean value;
    private UserDto user;
    private LocalDateTime completeDate;
    private Integer daysToFinish;
}
