package cz.bbn.cerberus.enumeration.factory;

import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationTypeDto;
import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationEntity;
import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationTypeEntity;

public class EnumerationFactory {

    public static EnumerationTypeDto fromEntity(EnumerationTypeEntity enumerationTypeEntity){
        EnumerationTypeDto dto = new EnumerationTypeDto();
        dto.setId(enumerationTypeEntity.getId());
        dto.setName(enumerationTypeEntity.getName());
        dto.setDescription(enumerationTypeEntity.getDescription());
        dto.setTranslationKey(enumerationTypeEntity.getTranslationKey());
        dto.setPermissionKey(enumerationTypeEntity.getPermissionKey());
        return dto;
    }

    public static EnumerationDto fromEntity(EnumerationEntity enumerationEntity){
        EnumerationDto dto = new EnumerationDto();
        dto.setId(enumerationEntity.getId());
        dto.setName(enumerationEntity.getName());
        dto.setDescription(enumerationEntity.getDescription());
        dto.setEnumerationTypeDto(fromEntity(enumerationEntity.getEnumerationTypeEntity()));
        dto.setValue(enumerationEntity.getValue());
        dto.setAllowed(enumerationEntity.getAllowed());
        dto.setDeleted(enumerationEntity.getDeleted());
        return dto;
    }

    public static void fillEntity(EnumerationEntity entity, EnumerationDto dto){
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setDescription(dto.getDescription());
        entity.setAllowed(dto.getAllowed());
        entity.setValue(dto.getValue());
        entity.setDeleted(dto.getDeleted());

        EnumerationTypeEntity enumerationTypeEntity = new EnumerationTypeEntity();
        fillEntityEnumerationType(enumerationTypeEntity, dto.getEnumerationTypeDto());
        entity.setEnumerationTypeEntity(enumerationTypeEntity);
    }

    public static void fillEntityEnumerationType(EnumerationTypeEntity entity, EnumerationTypeDto dto){
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setPermissionKey(dto.getPermissionKey());
        entity.setTranslationKey(dto.getTranslationKey());
        entity.setDescription(dto.getDescription());
    }
}
