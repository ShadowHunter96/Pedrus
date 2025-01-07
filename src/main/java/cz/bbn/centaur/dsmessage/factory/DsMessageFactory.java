package cz.bbn.cerberus.dsmessage.factory;

import cz.bbn.cerberus.dsmessage.dto.DsMessageAttachementDto;
import cz.bbn.cerberus.dsmessage.dto.DsMessageDto;
import cz.bbn.cerberus.dsmessage.dto.DsMessageSimpleDto;
import cz.bbn.cerberus.dsmessage.persistance.entity.DsMessageAttachementEntity;
import cz.bbn.cerberus.dsmessage.persistance.entity.DsMessageEntity;
import cz.bbn.cerberus.dsmessage.persistance.entity.DsMessageSimpleEntity;

import java.util.ArrayList;
import java.util.List;

public class DsMessageFactory {

    private DsMessageFactory() {
    }

    public static DsMessageSimpleDto fromEntity(DsMessageSimpleEntity entity){
        DsMessageSimpleDto dto = new DsMessageSimpleDto();
        dto.setId(entity.getId());
        dto.setSenderName(entity.getSenderName());
        dto.setSubject(entity.getSubject());
        dto.setAttachementSize(entity.getAttachementSize());
        dto.setViewed(entity.getViewed());
        dto.setDeliveryTime(entity.getDeliveryTime());
        dto.setCreatedInAppTime(entity.getCreatedInAppTime());
        dto.setRecipientId(entity.getRecipientId());
        dto.setDeleted(entity.getDeleted());
        dto.setType(entity.getType());
        return dto;
    }

    public static DsMessageDto fromEntity(DsMessageEntity entity){
        DsMessageDto dto = new DsMessageDto();
        dto.setId(entity.getId());
        dto.setSenderId(entity.getSenderId());
        dto.setSenderName(entity.getSenderName());
        dto.setSenderAddress(entity.getSenderAddress());
        dto.setSubject(entity.getSubject());
        dto.setMessage(entity.getMessage());
        dto.setAttachementSize(entity.getAttachementSize());
        dto.setViewed(entity.getViewed());
        dto.setDeliveryTime(entity.getDeliveryTime());
        dto.setRecipientId(entity.getRecipientId());
        dto.setAcceptanceTime(entity.getAcceptanceTime());
        dto.setCreatedInAppTime(entity.getCreatedInAppTime());
        dto.setDeleted(entity.getDeleted());
        dto.setType(entity.getType());

        List<DsMessageAttachementDto> dsMessageAttachementDtoList = new ArrayList<>();
        if(entity.getDsMessageAttachementEntityList() != null){
            entity.getDsMessageAttachementEntityList().forEach(dsMessageAttachementEntity ->
                dsMessageAttachementDtoList.add(getDsMessageAttachementDto(dsMessageAttachementEntity))
            );
        }
        dto.setDsMessageAttachementDtoList(dsMessageAttachementDtoList);
        return dto;
    }

    private static DsMessageAttachementDto getDsMessageAttachementDto(DsMessageAttachementEntity entity){
        DsMessageAttachementDto dto = new DsMessageAttachementDto();
        dto.setId(entity.getId());
        dto.setMessageId(entity.getMessageId());
        dto.setName(entity.getName());
        return dto;
    }

    public static void fillEntity(DsMessageEntity entity, DsMessageDto dsMessageDto) {
        entity.setDeleted(dsMessageDto.getDeleted());
        entity.setViewed(dsMessageDto.getViewed());
    }
}
