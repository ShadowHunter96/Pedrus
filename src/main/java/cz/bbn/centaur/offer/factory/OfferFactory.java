package cz.bbn.cerberus.offer.factory;

import cz.bbn.cerberus.subject.factory.SubjectFactory;
import cz.bbn.cerberus.subject.persistance.SubjectEntity;
import cz.bbn.cerberus.offer.dto.OfferDto;
import cz.bbn.cerberus.offer.repository.entity.OfferEntity;
import cz.bbn.cerberus.opportunity.factory.OpportunityFactory;
import cz.bbn.cerberus.opportunity.persistance.entity.OpportunityEntity;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;

import java.time.LocalDateTime;

public class OfferFactory {

    private OfferFactory() {
    }

    public static OfferDto fromEntity(OfferEntity entity){
        OfferDto dto = new OfferDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setOpportunityDto(OpportunityFactory.fromEntity(entity.getOpportunityEntity()));
        dto.setPriceWithoutVat(entity.getPriceWithoutVat());
        dto.setAppCurrency(entity.getAppCurrency());
        dto.setOfferDate(entity.getOfferDate());
        dto.setValidityDate(entity.getValidityDate());
        dto.setCustomerReference(entity.getCustomerReference());
        dto.setMarketUrl(entity.getMarketUrl());
        dto.setSvnUrl(entity.getSvnUrl());
        if(entity.getProcessedByUserEntity() != null){
            dto.setProcessedByUserDto(UserFactory.fromEntity(entity.getProcessedByUserEntity()));
        }
        if(entity.getSubjectEntity() != null){
            dto.setSubjectDto(SubjectFactory.fromEntity(entity.getSubjectEntity()));
        }
        if(entity.getOwnOrganizationSubjectEntity() != null){
            dto.setOwnOrganizationSubjectDto(SubjectFactory.fromEntity(entity.getOwnOrganizationSubjectEntity()));
        }
        dto.setAssurance(entity.getAssurance());
        dto.setPriceAssurance(entity.getPriceAssurance());
        dto.setSent(entity.getSent());
        dto.setExplanation(entity.getExplanation());
        dto.setState(entity.getState());
        dto.setDeleted(entity.getDeleted());
        dto.setLastUpdate(entity.getLastUpdate());
        dto.setSequence(entity.getSequence());
        return dto;
    }

    public static void fillEntity(OfferEntity entity, OfferDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());

        OpportunityEntity opportunityEntity = new OpportunityEntity();
        OpportunityFactory.fillEntity(opportunityEntity, dto.getOpportunityDto());
        entity.setOpportunityEntity(opportunityEntity);

        entity.setPriceWithoutVat(dto.getPriceWithoutVat());
        entity.setAppCurrency(dto.getAppCurrency());
        entity.setOfferDate(dto.getOfferDate());
        entity.setValidityDate(dto.getValidityDate());
        entity.setCustomerReference(dto.getCustomerReference());
        entity.setMarketUrl(dto.getMarketUrl());
        entity.setSvnUrl(dto.getSvnUrl());
        if(dto.getProcessedByUserDto() != null){
            UserEntity userEntity = new UserEntity();
            UserFactory.fillEntity(userEntity, dto.getProcessedByUserDto());
            entity.setProcessedByUserEntity(userEntity);
        }
        if(dto.getSubjectDto() != null){
            SubjectEntity subjectEntity = new SubjectEntity();
            SubjectFactory.fillEntity(subjectEntity, dto.getSubjectDto());
            entity.setSubjectEntity(subjectEntity);
        }
        if(dto.getOwnOrganizationSubjectDto() != null){
            SubjectEntity ownOrganizationSubjectEntity = new SubjectEntity();
            SubjectFactory.fillEntity(ownOrganizationSubjectEntity, dto.getOwnOrganizationSubjectDto());
            entity.setOwnOrganizationSubjectEntity(ownOrganizationSubjectEntity);
        }
        entity.setAssurance(dto.getAssurance());
        entity.setPriceAssurance(dto.getPriceAssurance());
        entity.setSent(dto.getSent());
        entity.setExplanation(dto.getExplanation());
        entity.setState(dto.getState());
        entity.setDeleted(dto.getDeleted());
        entity.setLastUpdate(LocalDateTime.now());
        entity.setSequence(dto.getSequence());
    }
}
