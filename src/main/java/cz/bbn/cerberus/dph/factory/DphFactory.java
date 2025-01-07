package cz.bbn.cerberus.dph.factory;

import cz.bbn.cerberus.dph.dto.DphDto;
import cz.bbn.cerberus.dph.persistance.entity.DphEntity;

public class DphFactory {

    private DphFactory() {
    }

    public static DphDto fromEntity(DphEntity entity) {
        DphDto dto = new DphDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setValue(entity.getValue());
        dto.setAllowed(entity.getAllowed());
        dto.setDefaultValue(entity.getDefaultValue());
        return dto;
    }

    public static void fillEntity(DphEntity entity, DphDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setValue(dto.getValue());
        entity.setAllowed(dto.getAllowed());
        entity.setDefaultValue(dto.getDefaultValue());
    }
}
