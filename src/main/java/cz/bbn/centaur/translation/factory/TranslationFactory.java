package cz.bbn.cerberus.translation.factory;

import cz.bbn.cerberus.translation.dto.TranslationDto;
import cz.bbn.cerberus.translation.persistence.TranslationEntity;

public class TranslationFactory {

    public static TranslationDto fromEntity(TranslationEntity entity) {
        TranslationDto dto = new TranslationDto();
        dto.setId(entity.getId());
        dto.setLang(entity.getLang());
        dto.setKey(entity.getKey());
        dto.setValue(entity.getValue());
        return dto;
    }

    public static void fillEntity(TranslationEntity entity, TranslationDto dto) {
        entity.setId(dto.getId());
        entity.setLang(dto.getLang());
        entity.setKey(dto.getKey());
        entity.setValue(dto.getValue());
    }
}
