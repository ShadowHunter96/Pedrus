package cz.bbn.cerberus.phase.factory;

import cz.bbn.cerberus.phase.dto.PhaseDto;
import cz.bbn.cerberus.phase.repository.PhaseEntity;

public class PhaseFactory {

    private PhaseFactory(){

    }

    public static PhaseDto fromEntity(PhaseEntity entity){
        PhaseDto dto = new PhaseDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setProjectId(entity.getProjectId());
        return dto;
    }

    public static void fillEntity(PhaseEntity entity, PhaseDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setProjectId(dto.getProjectId());
    }
}
