package cz.bbn.cerberus.taskfollowing.factory;

import cz.bbn.cerberus.taskfollowing.dto.TaskFollowingDto;
import cz.bbn.cerberus.taskfollowing.persistance.TaskFollowingEntity;
import cz.bbn.cerberus.taskfollowing.persistance.TaskFollowingId;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;

import java.util.HashSet;


public class TaskFollowingFactory {

    private TaskFollowingFactory() {
    }

    public static TaskFollowingDto fromEntity(TaskFollowingEntity entity) {
        TaskFollowingDto dto = new TaskFollowingDto();
        dto.setUserDto(UserFactory.fromEntity(entity.getId().getUserEntity()));
        dto.setFollowingUserDto(UserFactory.fromEntity(entity.getId().getFollowingUserEntity()));
        dto.setTaskDtoList(new HashSet<>());
        return dto;
    }

    public static void fillEntity(TaskFollowingEntity entity, TaskFollowingDto dto) {
        UserEntity userEntity = new UserEntity();
        UserFactory.fillEntity(userEntity, dto.getUserDto());

        UserEntity followingUserEntity = new UserEntity();
        UserFactory.fillEntity(followingUserEntity, dto.getFollowingUserDto());

        TaskFollowingId taskFollowingId = new TaskFollowingId(userEntity, followingUserEntity);
        entity.setId(taskFollowingId);
    }
}
