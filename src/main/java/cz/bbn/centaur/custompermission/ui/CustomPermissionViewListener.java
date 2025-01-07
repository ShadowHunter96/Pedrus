package cz.bbn.cerberus.custompermission.ui;

import cz.bbn.cerberus.custompermission.dto.CustomUserPermissionDto;
import cz.bbn.cerberus.user.dto.UserDto;

import java.util.Set;

public interface CustomPermissionViewListener {

    void setCustomPermissionDtoList(Set<CustomUserPermissionDto> customPermissionSet, String object,
                                    UserDto user, String objectId);

    Set<CustomUserPermissionDto> getUserPermissions(String object, UserDto userName, String objectId);

    void saveAll();
}
