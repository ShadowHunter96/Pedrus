package cz.bbn.cerberus.role.factory;

import cz.bbn.cerberus.role.dto.RoleHasPermissionDto;
import cz.bbn.cerberus.role.persistance.entity.RoleHasPermissionEntity;
import cz.bbn.cerberus.role.persistance.entity.RoleHasPermissionId;

public class RoleHasPermissionFactory {

    private RoleHasPermissionFactory() {
    }

    public static RoleHasPermissionDto fromEntity(RoleHasPermissionEntity entity) {
        return new RoleHasPermissionDto(entity.getId().getRoleId(), entity.getId().getPermissionId());
    }

    public static RoleHasPermissionEntity fillRoleHasPermission(
            String roleId, RoleHasPermissionDto roleHasPermissionDto) {
        RoleHasPermissionId id = new RoleHasPermissionId();
        id.setRoleId(roleId);
        id.setPermissionId(roleHasPermissionDto.getPermissionId());
        return new RoleHasPermissionEntity(id);
    }
}
