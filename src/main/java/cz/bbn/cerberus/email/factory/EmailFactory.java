package cz.bbn.cerberus.email.factory;

import cz.bbn.cerberus.email.dto.EmailDto;
import cz.bbn.cerberus.email.dto.EmailSimpleDto;
import cz.bbn.cerberus.email.persistence.entity.EmailEntity;
import cz.bbn.cerberus.email.persistence.entity.EmailSimpleEntity;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

public class EmailFactory {

    private EmailFactory() {
    }

    public static EmailDto fromEntity(EmailEntity entity) {
        EmailDto dto = new EmailDto();
        dto.setId(entity.getId());
        dto.setSubject(entity.getSubject());
        dto.setBody(entity.getBody());
        dto.setSender(entity.getSender());
        dto.setDateAndTime(entity.getDateAndTime());
        dto.setNoOfAttachments(entity.getNoOfAttachments());
        if (entity.getFile() != null) {
            InputStream inputStream = new ByteArrayInputStream(entity.getFile());
            dto.setFile(inputStream);
        }
        dto.setCustomer(entity.getCustomer());
        dto.setEntityType(entity.getEntityType());
        dto.setEntityId(entity.getEntityId());
        return dto;
    }

    public static void fillEntity(EmailEntity entity, EmailDto dto) throws IOException {
        entity.setId(dto.getId());
        entity.setSubject(dto.getSubject());
        entity.setBody(dto.getBody());
        entity.setSender(dto.getSender());
        entity.setDateAndTime(dto.getDateAndTime());
        entity.setNoOfAttachments(dto.getNoOfAttachments());
        entity.setFile(dto.getFile().readAllBytes());
        entity.setCustomer(dto.getCustomer());
        entity.setEntityType(dto.getEntityType());
        entity.setEntityId(dto.getEntityId());
    }

    public static EmailSimpleDto fromEntity(EmailSimpleEntity entity) {
        EmailSimpleDto dto = new EmailSimpleDto();
        dto.setId(entity.getId());
        dto.setSubject(entity.getSubject());
        dto.setBody(entity.getBody());
        dto.setSender(entity.getSender());
        dto.setDateAndTime(entity.getDateAndTime());
        dto.setNoOfAttachments(entity.getNoOfAttachments());
        dto.setCustomer(entity.getCustomer());
        dto.setEntityType(entity.getEntityType());
        dto.setEntityId(entity.getEntityId());
        return dto;
    }

    public static EmailSimpleDto simpleFromRegularDto(EmailDto emailDto) {
        EmailSimpleDto emailSimpleDto = new EmailSimpleDto();
        emailSimpleDto.setId(emailDto.getId());
        emailSimpleDto.setSubject(emailDto.getSubject());
        emailSimpleDto.setBody(emailDto.getBody());
        emailSimpleDto.setSender(emailDto.getSender());
        emailSimpleDto.setDateAndTime(emailDto.getDateAndTime());
        emailSimpleDto.setNoOfAttachments(emailDto.getNoOfAttachments());
        emailSimpleDto.setCustomer(emailDto.getCustomer());
        emailSimpleDto.setEntityType(emailDto.getEntityType());
        emailSimpleDto.setEntityId(emailDto.getEntityId());
        return emailSimpleDto;
    }

}
