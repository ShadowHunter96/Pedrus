package cz.bbn.cerberus.role.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.util.Set;

@Getter
@Setter
@ToString
public class RoleDto implements Serializable {

    private String id;
    private String description;

    private Set<RoleHasPermissionDto> roleHasPermissionSet;

    private Boolean backOffice;
    private Boolean infrastructure;
}
