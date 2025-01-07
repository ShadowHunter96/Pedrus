package cz.bbn.cerberus.tasktype.dto;

import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.role.dto.RoleDto;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.Set;

@Getter
@Setter
public class TaskTypeDto implements Serializable {

    private Long id;
    private String name;
    private String description;
    private Boolean archived;
    private Set<ObjectType> objectTypeSet;
    private Set<RoleDto> roleSet;
}
