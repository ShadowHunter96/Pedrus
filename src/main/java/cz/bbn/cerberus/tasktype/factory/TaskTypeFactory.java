package cz.bbn.cerberus.tasktype.factory;

import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.role.factory.RoleFactory;
import cz.bbn.cerberus.tasktype.dto.TaskTypeDto;
import cz.bbn.cerberus.tasktype.persistance.TaskTypeEntity;
import cz.bbn.cerberus.tasktype.persistance.TaskTypeRoleEntity;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class TaskTypeFactory {

    public static TaskTypeDto fromEntity(TaskTypeEntity entity) {
        TaskTypeDto dto = new TaskTypeDto();

        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setDescription(entity.getDescription());
        dto.setArchived(Boolean.TRUE.equals(entity.getArchived()));

        Set<ObjectType> objectTypeSet = new HashSet<>();
        if (entity.getObjectType() != null && !entity.getObjectType().isEmpty()) {
            objectTypeSet.addAll(Arrays.stream(entity.getObjectType().split(";")).map(ObjectType::valueOf).toList());
        }
        dto.setObjectTypeSet(objectTypeSet);

        Set<TaskTypeRoleEntity> taskRoleEntitySet = entity.getTaskTypeRoleEntitySet();
        Set<RoleDto> roleDtoSet = new HashSet<>();
        taskRoleEntitySet.forEach(taskTypeRoleEntity -> {
            RoleDto roleDto = RoleFactory.fromEntity(taskTypeRoleEntity.getId().getRoleEntity());
            roleDtoSet.add(roleDto);
        });
        dto.setRoleSet(roleDtoSet);
        return dto;
    }

    public static void fillEntity(TaskTypeEntity entity, TaskTypeDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setDescription(dto.getDescription());
        entity.setArchived(dto.getArchived());
        entity.setObjectType(StringUtils.join(dto.getObjectTypeSet(), ";"));
    }
}
