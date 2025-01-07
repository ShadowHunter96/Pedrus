package cz.bbn.cerberus.virtualserver.factory;

import cz.bbn.cerberus.virtualserver.dto.HddDto;
import cz.bbn.cerberus.virtualserver.persistance.HddEntity;

public class HddFactory {

    private HddFactory() {
    }

    public static HddDto fromEntity(HddEntity entity) {
        HddDto dto = new HddDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setSize(entity.getSize());
        dto.setVirtualServerId(entity.getVirtualServerId());
        return dto;
    }

    public static void fillEntity(HddEntity entity, HddDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setSize(dto.getSize());
        entity.setVirtualServerId(dto.getVirtualServerId());
    }
}
