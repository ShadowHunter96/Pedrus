package cz.bbn.cerberus.task.factory;

import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.role.factory.RoleFactory;
import cz.bbn.cerberus.task.dto.NotifyFrequency;
import cz.bbn.cerberus.task.dto.SendTask;
import cz.bbn.cerberus.task.dto.TaskColor;
import cz.bbn.cerberus.task.dto.TaskDto;
import cz.bbn.cerberus.task.dto.TaskState;
import cz.bbn.cerberus.task.persitance.entity.TaskEntity;
import cz.bbn.cerberus.task.persitance.entity.TaskRoleEntity;
import cz.bbn.cerberus.task.persitance.entity.TaskUserEntity;
import cz.bbn.cerberus.tasktype.factory.TaskTypeFactory;
import cz.bbn.cerberus.tasktype.persistance.TaskTypeEntity;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class TaskFactory {

    private TaskFactory() {
    }

    public static TaskDto fromEntity(TaskEntity entity) {
        return fromEntity(entity, false);
    }

    public static TaskDto fromEntity(TaskEntity entity, boolean loadUsers) {
        TaskDto dto = new TaskDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setDescription(entity.getDescription());
        dto.setDate(entity.getDate());
        dto.setObjectType(entity.getObjectType());
        dto.setObjectId(entity.getObjectId());
        dto.setState(TaskState.getFromNameOrDefault(entity.getState()));

        dto.setDaysToNotify(entity.getDaysToNotify());
        dto.setNotifyFrequency(NotifyFrequency.getFromNameOrDefault(entity.getNotifyFrequency()));
        dto.setColor(TaskColor.getFromNameOrDefault(entity.getColor()));

        if (entity.getUserEntity() != null) {
            dto.setUserDto(UserFactory.fromEntity(entity.getUserEntity()));
        }
        if (loadUsers) {
            Set<TaskUserEntity> taskUserEntitySet = entity.getTaskUserEntitySet();
            Set<UserDto> userDtoSet = new HashSet<>();
            taskUserEntitySet.forEach(taskUserEntity -> {
                UserDto userDto = UserFactory.fromEntity(taskUserEntity.getId().getUserEntity());
                userDtoSet.add(userDto);
            });
            dto.setUserDtoSet(userDtoSet);

            Set<TaskRoleEntity> taskRoleEntitySet = entity.getTaskRoleEntitySet();
            Set<RoleDto> roleDtoSet = new HashSet<>();
            taskRoleEntitySet.forEach(taskRoleEntity -> {
                RoleDto roleDto = RoleFactory.fromEntity(taskRoleEntity.getId().getRoleEntity());
                roleDtoSet.add(roleDto);
            });
            dto.setRoleDtoSet(roleDtoSet);
        }
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
        dto.setSubjectId(entity.getSubjectId());
        dto.setCreationDate(entity.getCreationDate());
        if (entity.getAssignee() != null) {
            dto.setAssignee(UserFactory.fromEntity(entity.getAssignee()));
        }
        if (entity.getTaskType() != null) {
            dto.setTaskType(TaskTypeFactory.fromEntity(entity.getTaskType()));
        }
        dto.setDeleted(Boolean.TRUE.equals(entity.getDeleted()));
        return dto;
    }


    public static void fillEntity(TaskEntity entity, TaskDto taskDto) {
        entity.setId(taskDto.getId());
        entity.setName(taskDto.getName());
        entity.setDescription(taskDto.getDescription());
        entity.setDate(taskDto.getDate());
        entity.setObjectType(taskDto.getObjectType());
        entity.setObjectId(taskDto.getObjectId());

        entity.setDaysToNotify(taskDto.getDaysToNotify());

        if (taskDto.getColor() != null) {
            entity.setColor(taskDto.getColor().name());
        }

        if (taskDto.getNotifyFrequency() != null) {
            entity.setNotifyFrequency(taskDto.getNotifyFrequency().name());
        }

        if (taskDto.getState() != null) {
            entity.setState(taskDto.getState().name());
        }

        if (taskDto.getUserDto() != null) {
            UserEntity userEntity = new UserEntity();
            UserFactory.fillEntity(userEntity, taskDto.getUserDto());
            entity.setUserEntity(userEntity);
        }

        entity.setSendTask(StringUtils.join(taskDto.getSendTaskSet(), ","));
        entity.setSubjectId(taskDto.getSubjectId());
        entity.setCreationDate(taskDto.getCreationDate());

        if (taskDto.getAssignee() != null) {
            UserEntity userEntity = new UserEntity();
            UserFactory.fillEntity(userEntity, taskDto.getAssignee());
            entity.setAssignee(userEntity);
        }
        if (taskDto.getTaskType() != null) {
            TaskTypeEntity taskTypeEntity = new TaskTypeEntity();
            TaskTypeFactory.fillEntity(taskTypeEntity, taskDto.getTaskType());
            entity.setTaskType(taskTypeEntity);
        }
        entity.setDeleted(Boolean.TRUE.equals(taskDto.getDeleted()));
    }
}
