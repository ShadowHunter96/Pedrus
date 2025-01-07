package cz.bbn.cerberus.project.dto;

import cz.bbn.cerberus.project.ProjectState;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
public class ProjectFilterDto {

    private String id;
    private String name;
    private String subject;
    private String contract;
    private boolean showDeleted;
    private String objectId;
    private UserDto userDto;
    private ProjectState state;
    private boolean onlyEditPermission;

    private LocalDateTime searchStart;
    private LocalDateTime searchEnd;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
