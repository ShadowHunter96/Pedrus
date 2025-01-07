package cz.bbn.cerberus.custompermission.dto;

import cz.bbn.cerberus.user.dto.UserDto;
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
public class ObjectUserPermissionDto {

    private UserDto user;

    private String object;

    private Set<CustomUserPermissionDto> permissionSet;

    private String objectId;
}
