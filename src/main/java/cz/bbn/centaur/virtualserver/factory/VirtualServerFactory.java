package cz.bbn.cerberus.virtualserver.factory;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.enumeration.factory.EnumerationFactory;
import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationEntity;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;
import cz.bbn.cerberus.virtualserver.dto.HddDto;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerDto;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerNotificationPeriod;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerStatus;
import cz.bbn.cerberus.virtualserver.persistance.VirtualServerEntity;

import java.util.List;

public class VirtualServerFactory {

    private VirtualServerFactory() {
    }

    public static VirtualServerDto fromEntity(VirtualServerEntity entity) {
        VirtualServerDto dto = new VirtualServerDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setOs(entity.getOs());
        dto.setCpu(entity.getCpu());
        dto.setCores(entity.getCores());
        dto.setRam(entity.getRam());
        dto.setIp(entity.getIp());
        if (entity.getOwner() != null) {
            dto.setOwner(UserFactory.fromEntity(entity.getOwner()));
        }
        dto.setCreationDate(entity.getCreationDate());
        dto.setRequestDate(entity.getRequestDate());
        dto.setStatus(VirtualServerStatus.getFromNameOrNonExistent(entity.getStatus()));
        dto.setDeleted(entity.getDeleted());
        if (entity.getHddEntityList() != null && !entity.getHddEntityList().isEmpty()) {
            List<HddDto> hddList = ConvertEntities.fromEntities(entity.getHddEntityList(), HddFactory::fromEntity);
            dto.setHddDtoList(hddList);
        }
        if (entity.getSubnet() != null) {
            dto.setSubnet(EnumerationFactory.fromEntity(entity.getSubnet()));
        }
        dto.setStringId(entity.getStringId());
        dto.setNotificationPeriod(VirtualServerNotificationPeriod.getFromName(entity.getNotificationPeriod()));
        return dto;
    }

    public static void fillEntity(VirtualServerEntity entity, VirtualServerDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setOs(dto.getOs());
        entity.setCpu(dto.getCpu());
        entity.setCores(dto.getCores());
        entity.setRam(dto.getRam());
        entity.setIp(dto.getIp());
        if (dto.getOwner() != null) {
            UserEntity user = new UserEntity();
            UserFactory.fillEntity(user, dto.getOwner());
            entity.setOwner(user);
        }
        entity.setCreationDate(dto.getCreationDate());
        entity.setRequestDate(dto.getRequestDate());
        if (dto.getStatus() != null) {
            entity.setStatus(dto.getStatus().name());
        }
        entity.setDeleted(dto.getDeleted());
        if (dto.getSubnet() != null) {
            EnumerationEntity enumEntity = new EnumerationEntity();
            EnumerationFactory.fillEntity(enumEntity, dto.getSubnet());
            entity.setSubnet(enumEntity);
        }
        entity.setStringId(dto.getStringId());
        if (dto.getNotificationPeriod() != null) {
            entity.setNotificationPeriod(dto.getNotificationPeriod().name());
        }
    }
}
