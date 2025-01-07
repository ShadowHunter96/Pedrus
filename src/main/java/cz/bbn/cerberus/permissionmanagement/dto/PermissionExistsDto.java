package cz.bbn.cerberus.permissionmanagement.dto;

import lombok.Getter;

@Getter
public class PermissionExistsDto {

    private final boolean isPermission;
    private final String permissionName;

    public PermissionExistsDto(boolean isPermission, String permissionName) {
        this.isPermission = isPermission;
        this.permissionName = permissionName;
    }
}
