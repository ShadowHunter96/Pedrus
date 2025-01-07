package cz.bbn.cerberus.tasktemplate.factory;

import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.role.factory.RoleFactory;
import cz.bbn.cerberus.role.persistance.entity.RoleEntity;
import cz.bbn.cerberus.task.dto.NotifyFrequency;
import cz.bbn.cerberus.task.dto.SendTask;
import cz.bbn.cerberus.task.dto.TaskColor;
import cz.bbn.cerberus.task.dto.TaskState;
import cz.bbn.cerberus.tasktemplate.dto.TaskTemplateDto;
import cz.bbn.cerberus.tasktemplate.persistance.entity.TaskTemplateEntity;
import cz.bbn.cerberus.tasktemplate.persistance.entity.TaskTemplateRoleEntity;
import cz.bbn.cerberus.tasktemplate.persistance.entity.TaskTemplateUserEntity;
import cz.bbn.cerberus.tasktype.factory.TaskTypeFactory;
import cz.bbn.cerberus.tasktype.persistance.TaskTypeEntity;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class TaskTemplateFactory {

    public static TaskTemplateDto fromEntity(TaskTemplateEntity entity) {
        TaskTemplateDto dto = new TaskTemplateDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setDescription(entity.getDescription());
        dto.setObjectType(entity.getObjectType());
        dto.setObjectId(entity.getObjectId());
        if (entity.getUserEntity() != null) {
            dto.setUserDto(UserFactory.fromEntity(entity.getUserEntity()));
        }
        dto.setState(TaskState.getFromNameOrDefault(entity.getState()));
        dto.setDaysToNotify(entity.getDaysToNotify());
        dto.setNotifyFrequency(NotifyFrequency.getFromNameOrDefault(entity.getNotifyFrequency()));
        dto.setColor(TaskColor.getFromNameOrDefault(entity.getColor()));
        if (entity.getAssignee() != null) {
            dto.setAssignee(UserFactory.fromEntity(entity.getAssignee()));
        }
        if (entity.getAllowedRole() != null) {
            dto.setAllowedRole(RoleFactory.fromEntity(entity.getAllowedRole()));
        }
        Set<TaskTemplateUserEntity> taskUserEntitySet = entity.getTaskTemplateUserEntitySet();
        Set<UserDto> userDtoSet = new HashSet<>();
        taskUserEntitySet.forEach(taskTemplateUserEntity -> {
            UserDto userDto = UserFactory.fromEntity(taskTemplateUserEntity.getId().getUserEntity());
            userDtoSet.add(userDto);
        });
        dto.setUserDtoSet(userDtoSet);
        Set<SendTask> sendTaskSet = new HashSet<>();
        if (entity.getSendTask() != null) {
            String[] sendTasks = entity.getSendTask().split(",");
            Arrays.stream(sendTasks).forEach(s -> {
                if (!s.isEmpty()) {
                    sendTaskSet.add(SendTask.valueOf(s));
                }
            });
        }
        dto.setSendTaskSet(sendTaskSet);
        Set<TaskTemplateRoleEntity> taskTemplateRoleEntitySet = entity.getTaskTemplateRoleEntitySet();
        Set<RoleDto> roleDtoSet = new HashSet<>();
        taskTemplateRoleEntitySet.forEach(taskTemplateRoleEntity -> {
            RoleDto roleDto = RoleFactory.fromEntity(taskTemplateRoleEntity.getId().getRoleEntity());
            roleDtoSet.add(roleDto);
        });
        dto.setRoleDtoSet(roleDtoSet);
        dto.setDeleted(Boolean.TRUE.equals(entity.getDeleted()));
        dto.setSendToOutlook(Boolean.TRUE.equals(entity.getSendToOutlook()));
        if (entity.getTaskType() != null) {
            dto.setTaskType(TaskTypeFactory.fromEntity(entity.getTaskType()));
        }
        return dto;
    }

    public static void fillEntity(TaskTemplateEntity entity, TaskTemplateDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setDescription(dto.getDescription());
        entity.setObjectType(dto.getObjectType());
        entity.setObjectId(dto.getObjectId());
        entity.setSubjectId(dto.getSubjectId());
        if (dto.getUserDto() != null) {
            UserEntity userEntity = new UserEntity();
            UserFactory.fillEntity(userEntity, dto.getUserDto());
            entity.setUserEntity(userEntity);
        }
        if (dto.getState() != null) {
            entity.setState(dto.getState().name());
        }
        entity.setDaysToNotify(dto.getDaysToNotify());
        if (dto.getNotifyFrequency() != null) {
            entity.setNotifyFrequency(dto.getNotifyFrequency().name());
        }
        if (dto.getColor() != null) {
            entity.setColor(dto.getColor().name());
        }
        if (dto.getAssignee() != null) {
            UserEntity userEntity = new UserEntity();
            UserFactory.fillEntity(userEntity, dto.getAssignee());
            entity.setAssignee(userEntity);
        }
        if (dto.getAllowedRole() != null) {
            RoleEntity roleEntity = new RoleEntity();
            RoleFactory.fillEntity(roleEntity, dto.getAllowedRole());
            entity.setAllowedRole(roleEntity);
        }
        entity.setSendTask(StringUtils.join(dto.getSendTaskSet(), ","));
        entity.setDeleted(Boolean.TRUE.equals(dto.isDeleted()));
        entity.setSendToOutlook(Boolean.TRUE.equals(dto.isSendToOutlook()));
        if (dto.getTaskType() != null) {
            TaskTypeEntity taskTypeEntity = new TaskTypeEntity();
            TaskTypeFactory.fillEntity(taskTypeEntity, dto.getTaskType());
            entity.setTaskType(taskTypeEntity);
        }
    }
}
