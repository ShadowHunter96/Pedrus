package cz.bbn.cerberus.areatechnologysign.factory;

import cz.bbn.cerberus.area.factory.AreaFactory;
import cz.bbn.cerberus.area.persistance.entity.AreaEntity;
import cz.bbn.cerberus.areatechnologysign.AreaTechnologySignDto;
import cz.bbn.cerberus.areatechnologysign.persistance.AreaTechnologySignEntity;
import cz.bbn.cerberus.technology.factory.TechnologyFactory;
import cz.bbn.cerberus.technology.persistance.entity.TechnologyEntity;

public class AreaTechnologySignFactory {

    private AreaTechnologySignFactory() {
    }

    public static AreaTechnologySignDto fromEntity(AreaTechnologySignEntity entity) {
        AreaTechnologySignDto dto = new AreaTechnologySignDto();
        dto.setId(entity.getId());
        dto.setObjectType(entity.getObjectType());
        dto.setObjectId(entity.getObjectId());
        if (entity.getAreaEntity() != null) {
            dto.setAreaDto(AreaFactory.fromEntity(entity.getAreaEntity()));
        }
        if (entity.getTechnologyEntity() != null) {
            dto.setTechnologyDto(TechnologyFactory.fromEntity(entity.getTechnologyEntity()));
        }
        return dto;
    }

    public static void toEntity(AreaTechnologySignEntity entity, AreaTechnologySignDto dto) {
        if (dto.getAreaDto() != null) {
            AreaEntity areaEntity = new AreaEntity();
            AreaFactory.fillEntity(areaEntity, dto.getAreaDto());
            entity.setAreaEntity(areaEntity);
        }
        if (dto.getTechnologyDto() != null) {
            TechnologyEntity technologyEntity = new TechnologyEntity();
            TechnologyFactory.fillEntity(technologyEntity, dto.getTechnologyDto());
            entity.setTechnologyEntity(technologyEntity);
        }
        entity.setObjectId(dto.getObjectId());
        entity.setObjectType(dto.getObjectType());
    }
}
