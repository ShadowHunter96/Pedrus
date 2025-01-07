package cz.bbn.cerberus.rolecustompermission.dto;

import cz.bbn.cerberus.role.dto.RoleDto;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import java.util.Set;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class ObjectRolePermissionDto {

    private RoleDto role;

    private String object;

    private Set<CustomRolePermissionDto> permissionSet;

    private String objectId;
}
