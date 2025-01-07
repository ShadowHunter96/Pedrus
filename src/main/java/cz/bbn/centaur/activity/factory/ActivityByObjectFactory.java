package cz.bbn.cerberus.activity.factory;

import cz.bbn.cerberus.activity.dto.ActivityByObjectDto;
import cz.bbn.cerberus.activity.persistance.ActivityByObjectEntity;
import cz.bbn.cerberus.enumeration.factory.EnumerationFactory;

public class ActivityByObjectFactory {

    private ActivityByObjectFactory(){
    }

    public static ActivityByObjectDto fromEntity(ActivityByObjectEntity entity){
        ActivityByObjectDto dto = new ActivityByObjectDto();
        dto.setId(entity.getId());
        dto.setEnumerationDto(EnumerationFactory.fromEntity(entity.getEnumerationEntity()));
        return dto;
    }
}
