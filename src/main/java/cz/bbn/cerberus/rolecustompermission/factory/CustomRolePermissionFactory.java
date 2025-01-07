package cz.bbn.cerberus.rolecustompermission.factory;

import cz.bbn.cerberus.rolecustompermission.dto.CustomRolePermissionDto;
import cz.bbn.cerberus.rolecustompermission.persistance.CustomRolePermissionEntity;

public class CustomRolePermissionFactory {

    private CustomRolePermissionFactory() {
    }

    public static CustomRolePermissionDto fromEntity(CustomRolePermissionEntity entity) {
        CustomRolePermissionDto dto = new CustomRolePermissionDto();
        dto.setPermissionId(entity.getPermissionId());
        dto.setRoleId(entity.getRoleId());
        dto.setObjectName(entity.getObjectName());
        dto.setObjectId(entity.getObjectId());
        dto.setCanView(Boolean.TRUE.equals(entity.getCanView()));
        return dto;
    }

    public static void fillEntity(CustomRolePermissionEntity entity, CustomRolePermissionDto dto) {
        entity.setPermissionId(dto.getPermissionId());
        entity.setRoleId(dto.getRoleId());
        entity.setObjectName(dto.getObjectName());
        entity.setObjectId(dto.getObjectId());
        entity.setCanView(dto.isCanView());
    }
}
