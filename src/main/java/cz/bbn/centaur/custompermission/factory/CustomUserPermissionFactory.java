package cz.bbn.cerberus.custompermission.factory;

import cz.bbn.cerberus.custompermission.dto.CustomUserPermissionDto;
import cz.bbn.cerberus.custompermission.persistance.CustomUserPermissionEntity;

public class CustomUserPermissionFactory {

    private CustomUserPermissionFactory() {
    }

    public static CustomUserPermissionDto fromEntity(CustomUserPermissionEntity entity) {
        CustomUserPermissionDto dto = new CustomUserPermissionDto();
        dto.setPermissionId(entity.getPermissionId());
        dto.setUserId(entity.getUserId());
        dto.setObjectName(entity.getObjectName());
        dto.setObjectId(entity.getObjectId());
        dto.setCanView(Boolean.TRUE.equals(entity.getCanView()));
        return dto;
    }

    public static void fillEntity(CustomUserPermissionEntity entity, CustomUserPermissionDto dto) {
        entity.setPermissionId(dto.getPermissionId());
        entity.setUserId(dto.getUserId());
        entity.setObjectName(dto.getObjectName());
        entity.setObjectId(dto.getObjectId());
        entity.setCanView(dto.isCanView());
    }
}
