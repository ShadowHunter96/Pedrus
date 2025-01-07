package cz.bbn.cerberus.note.factory;

import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.note.dto.NoteDto;
import cz.bbn.cerberus.note.persistance.NoteEntity;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;


public class NoteFactory {

    private NoteFactory() {
    }

    public static NoteDto fromEntity(NoteEntity entity) {
        NoteDto dto = new NoteDto();
        dto.setId(entity.getId());
        dto.setNote(entity.getNote());
        dto.setDate(entity.getDate());
        dto.setPriority(entity.getPriority());
        dto.setEntityId(entity.getEntityId());
        dto.setType(NoteTypeEnum.getFromString(entity.getNoteTypeEnum()));
        dto.setArchived(entity.getArchived());
        dto.setUserDto(UserFactory.fromEntity(entity.getUserEntity()));
        return dto;
    }

    public static void fillEntity(NoteEntity entity, NoteDto dto) {
        entity.setId(dto.getId());
        entity.setNote(dto.getNote());
        entity.setDate(dto.getDate());
        entity.setPriority(dto.getPriority());
        entity.setNoteTypeEnum(dto.getType().name());
        entity.setEntityId(dto.getEntityId());
        entity.setArchived(dto.getArchived());

        UserEntity userEntity = new UserEntity();
        UserFactory.fillEntity(userEntity, dto.getUserDto());
        entity.setUserEntity(userEntity);
    }
}
