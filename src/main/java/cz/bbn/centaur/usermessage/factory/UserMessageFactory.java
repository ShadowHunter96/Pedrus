package cz.bbn.cerberus.usermessage.factory;

import cz.bbn.cerberus.usermessage.dto.UserMessageDto;
import cz.bbn.cerberus.usermessage.persistance.UserMessageEntity;

public class UserMessageFactory {


    private UserMessageFactory() {
    }

    public static UserMessageDto fromEntity(UserMessageEntity entity){
        UserMessageDto dto = new UserMessageDto();
        dto.setId(entity.getId());
        dto.setMessage(entity.getMessage());
        dto.setDueDate(entity.getDueDate());
        dto.setPriority(entity.getPriority());
        dto.setType(entity.getType());
        dto.setUserId(entity.getId());
        dto.setObjectId(entity.getObjectId());
        dto.setObjectType(entity.getObjectType());
        dto.setViewed(entity.getViewed());
        return dto;
    }

    public static UserMessageEntity fromDto(UserMessageDto dto){
        UserMessageEntity entity = new UserMessageEntity();
        entity.setMessage(dto.getMessage());
        entity.setUserId(dto.getUserId());
        entity.setDueDate(dto.getDueDate());
        entity.setObjectId(dto.getObjectId());
        entity.setObjectType(dto.getObjectType());
        entity.setPriority(dto.getPriority());
        entity.setType(dto.getType());
        entity.setViewed(dto.getViewed());
        return entity;
    }
}
