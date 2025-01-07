package cz.bbn.cerberus.task.factory;

import cz.bbn.cerberus.task.dto.TaskCheckDto;
import cz.bbn.cerberus.task.persitance.entity.TaskCheckEntity;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;

public class TaskCheckFactory {

    public static TaskCheckDto fromEntity(TaskCheckEntity entity) {
        TaskCheckDto dto = new TaskCheckDto();
        dto.setId(entity.getId());
        dto.setEntityType(entity.getEntityType());
        dto.setEntityId(entity.getEntityId());
        dto.setValue(entity.getValue());
        dto.setCheckboxName(entity.getCheckboxName());
        dto.setCompleteDate(entity.getCompleteDate());
        if (entity.getUser() != null) {
            dto.setUser(UserFactory.fromEntity(entity.getUser()));
        }
        dto.setDaysToFinish(entity.getDaysToFinish());
        return dto;
    }

    public static void fillEntity(TaskCheckEntity entity, TaskCheckDto dto) {
        entity.setId(dto.getId());
        entity.setEntityType(dto.getEntityType());
        entity.setEntityId(dto.getEntityId());
        entity.setValue(dto.getValue());
        entity.setCheckboxName(dto.getCheckboxName());
        entity.setCompleteDate(dto.getCompleteDate());
        if (dto.getUser() != null) {
            UserEntity userEntity = new UserEntity();
            UserFactory.fillEntity(userEntity, dto.getUser());
            entity.setUser(userEntity);
        }
        entity.setDaysToFinish(dto.getDaysToFinish());
    }
}
