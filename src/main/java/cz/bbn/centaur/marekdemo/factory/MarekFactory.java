package cz.bbn.cerberus.marekdemo.factory;

import cz.bbn.cerberus.marekdemo.dto.MarekDTO;

import cz.bbn.cerberus.marekdemo.entity.MarekEntity;

/**
 * Created by marek.vu on 04.10.2023.
 */
public class MarekFactory {
    private MarekFactory(){}

    public static MarekDTO fromEntity(MarekEntity entity){
        MarekDTO dto = new MarekDTO();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setDescription(entity.getDescription());
        return dto;
    }

    public static void fillEntity(MarekEntity entity, MarekDTO dto){
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setDescription(dto.getDescription());
    }


}
