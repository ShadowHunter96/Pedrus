package cz.bbn.cerberus.opportunity.factory;

import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.opportunity.dto.OpportunitySimpleDto;
import cz.bbn.cerberus.opportunity.persistance.entity.OpportunityEntity;
import cz.bbn.cerberus.opportunity.persistance.entity.OpportunitySimpleEntity;
import cz.bbn.cerberus.subject.factory.SubjectFactory;
import cz.bbn.cerberus.subject.persistance.SubjectEntity;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;

public class OpportunityFactory {

    private OpportunityFactory() {
    }

    public static OpportunitySimpleDto fromEntity(OpportunitySimpleEntity entity) {
        OpportunitySimpleDto dto = new OpportunitySimpleDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setProgress(entity.getProgress());
        dto.setSubject(entity.getSubjectId());
        dto.setState(entity.getState());
        dto.setDeleted(entity.getDeleted());
        dto.setUserId(entity.getUserId());
        return dto;
    }

    public static OpportunityDto fromEntity(OpportunityEntity entity) {
        OpportunityDto dto = new OpportunityDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setProgress(entity.getProgress());
        dto.setSuccessChance(entity.getSuccessChance());
        dto.setDescription(entity.getDescription());
        if (entity.getSubject() != null) {
            dto.setSubject(SubjectFactory.fromEntity(entity.getSubject()));
        }

        if (entity.getPrimarySupplier() != null) {
            dto.setPrimarySupplier(SubjectFactory.fromEntity(entity.getPrimarySupplier()));
        }

        dto.setVolume(entity.getVolume());
        dto.setAppCurrency(entity.getAppCurrency());
        dto.setStartDate(entity.getStartDate());
        dto.setState(entity.getState());
        dto.setCreateDate(entity.getCreateDate());

        if (entity.getWinnerSubject() != null) {
            dto.setWinnerSubject(SubjectFactory.fromEntity(entity.getWinnerSubject()));
        }

        dto.setWinningTechnology(entity.getWinningTechnology());

        if (entity.getUserEntity() != null) {
            dto.setUser(UserFactory.fromEntity(entity.getUserEntity()));
        }
        dto.setDeleted(entity.getDeleted());
        dto.setDateOfFulfilment(entity.getDateOfFulfilment());
        dto.setExpectedCosts(entity.getExpectedCosts());
        dto.setExpectedReturn(entity.getExpectedReturn());
        return dto;
    }

    public static void fillEntity(OpportunityEntity entity, OpportunityDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setDescription(dto.getDescription());
        entity.setProgress(dto.getProgress());
        entity.setSuccessChance(dto.getSuccessChance());

        SubjectEntity subjectEntity = new SubjectEntity();
        SubjectFactory.fillEntity(subjectEntity, dto.getSubject());
        entity.setSubject(subjectEntity);

        if (dto.getPrimarySupplier() != null) {
            SubjectEntity primarySubjectEntity = new SubjectEntity();
            SubjectFactory.fillEntity(primarySubjectEntity, dto.getPrimarySupplier());
            entity.setPrimarySupplier(primarySubjectEntity);
        }

        entity.setVolume(dto.getVolume());
        entity.setAppCurrency(dto.getAppCurrency());
        entity.setStartDate(dto.getStartDate());
        entity.setState(dto.getState());

        if (dto.getWinnerSubject() != null) {
            SubjectEntity winnerSubjectEntity = new SubjectEntity();
            SubjectFactory.fillEntity(winnerSubjectEntity, dto.getWinnerSubject());
            entity.setWinnerSubject(winnerSubjectEntity);
        }

        entity.setWinningTechnology(dto.getWinningTechnology());

        if (dto.getUser() != null) {
            UserEntity userEntity = new UserEntity();
            UserFactory.fillEntity(userEntity, dto.getUser());
            entity.setUserEntity(userEntity);
        }

        entity.setDeleted(dto.getDeleted());
        entity.setCreateDate(dto.getCreateDate());
        entity.setDateOfFulfilment(dto.getDateOfFulfilment());
        entity.setExpectedCosts(dto.getExpectedCosts());
        entity.setExpectedReturn(dto.getExpectedReturn());
    }

    public static ContractDto opportunityDtoToContractDto(OpportunityDto opportunityDto) {
        ContractDto contractDto = new ContractDto();
        contractDto.setId(opportunityDto.getId());
        contractDto.setName(opportunityDto.getName());
        contractDto.setDescription(opportunityDto.getDescription());
        contractDto.setSubjectDto(opportunityDto.getSubject());
        contractDto.setUserDto(opportunityDto.getUser());
        contractDto.setOpportunityDto(opportunityDto);
        return contractDto;
    }
}
