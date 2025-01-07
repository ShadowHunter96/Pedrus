package cz.bbn.cerberus.role.factory;

import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.role.persistance.entity.RoleEntity;

public class RoleFactory {

    private RoleFactory() {
    }

    public static RoleDto fromEntity(RoleEntity entity) {
        RoleDto roleDto = new RoleDto();
        roleDto.setId(entity.getId());
        roleDto.setDescription(entity.getName());
        roleDto.setBackOffice(entity.getBackOffice());
        roleDto.setInfrastructure(entity.getInfrastructure());
        return roleDto;
    }

    public static void fillEntity(RoleEntity roleEntity, RoleDto roleDto) {
        roleEntity.setId(roleDto.getId());
        roleEntity.setName(roleDto.getDescription());
        roleEntity.setBackOffice(roleDto.getBackOffice());
        roleEntity.setInfrastructure(roleDto.getInfrastructure());
    }
}
