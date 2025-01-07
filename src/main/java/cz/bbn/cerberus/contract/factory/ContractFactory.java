package cz.bbn.cerberus.contract.factory;

import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.contract.dto.ContractEndingDays;
import cz.bbn.cerberus.contract.dto.ContractInternalType;
import cz.bbn.cerberus.contract.persistence.entity.ContractEntity;
import cz.bbn.cerberus.contracttype.factory.ContractTypeFactory;
import cz.bbn.cerberus.contracttype.persistence.ContractTypeEntity;
import cz.bbn.cerberus.enumeration.factory.EnumerationFactory;
import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationEntity;
import cz.bbn.cerberus.opportunity.factory.OpportunityFactory;
import cz.bbn.cerberus.opportunity.persistance.entity.OpportunityEntity;
import cz.bbn.cerberus.subject.factory.SubjectFactory;
import cz.bbn.cerberus.subject.persistance.SubjectEntity;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;

import java.time.LocalDateTime;
import java.util.Arrays;

public class ContractFactory {

    private ContractFactory() {
    }

    public static ContractDto fromEntity(ContractEntity entity) {
        ContractDto dto = new ContractDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setDescription(entity.getDescription());
        dto.setEndContract(entity.getEndContract());
        dto.setDeleted(entity.getDeleted());
        if (entity.getSubject() != null) {
            dto.setSubjectDto(SubjectFactory.fromEntity(entity.getSubject()));
        }
        if (entity.getConnectedContract() != null) {
            dto.setConnectedContract(getConnectedEntity(entity.getConnectedContract()));
        }
        if (entity.getUserEntity() != null) {
            dto.setUserDto(UserFactory.fromEntity(entity.getUserEntity()));
        }
        dto.setContractSubject(entity.getContractSubject());
        dto.setValidityStart(entity.getValidityStart());
        dto.setEffectStart(entity.getEffectStart());
        dto.setAddition(entity.getAddition());
        if (entity.getContractParty() != null) {
            dto.setContractParty(SubjectFactory.fromEntity(entity.getContractParty()));
        }
        if (entity.getType() != null) {
            dto.setType(ContractTypeFactory.fromEntity(entity.getType()));
        }
        dto.setEvidenceNo(entity.getEvidenceNo());

        if (entity.getContractState() != null) {
            dto.setContractState(EnumerationFactory.fromEntity(entity.getContractState()));
        }
        dto.setPriceNoVat(entity.getPriceNoVat());
        dto.setMaturityInvoice(entity.getMaturityInvoice());

        if (entity.getOpportunityEntity() != null) {
            dto.setOpportunityDto(OpportunityFactory.fromEntity(entity.getOpportunityEntity()));
        }
        dto.setSigned(entity.getSigned());
        if (entity.getSendNotificationDaysBefore() != null) {
            dto.setSendNotificationDaysBefore(Arrays.stream(ContractEndingDays.values())
                    .filter(contractEndingDays ->
                            contractEndingDays.getDays() == entity.getSendNotificationDaysBefore())
                    .findAny().get());
        }
        dto.setLastUpdate(entity.getLastUpdate());
        dto.setAppCurrency(AppCurrency.valueOfOrCzk(entity.getAppCurrency()));
        dto.setInternalType(ContractInternalType.getValueOrEmpty(entity.getContractInternalType()));
        return dto;
    }

    private static ContractDto getConnectedEntity(ContractEntity entity) {
        ContractDto dto = new ContractDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setInternalType(ContractInternalType.getValueOrEmpty(entity.getContractInternalType()));
        return dto;
    }

    public static void fillEntity(ContractEntity entity, ContractDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setDescription(dto.getDescription());
        entity.setEndContract(dto.getEndContract());
        entity.setDeleted(false);

        if (dto.getSubjectDto() != null) {
            SubjectEntity subjectEntity = new SubjectEntity();
            SubjectFactory.fillEntity(subjectEntity, dto.getSubjectDto());
            entity.setSubject(subjectEntity);
        }

        if (dto.getConnectedContract() != null) {
            ContractEntity contractEntity = new ContractEntity();
            ContractFactory.fillEntity(contractEntity, dto.getConnectedContract());
            entity.setConnectedContract(contractEntity);
        }

        if (dto.getUserDto() != null) {
            UserEntity userEntity = new UserEntity();
            UserFactory.fillEntity(userEntity, dto.getUserDto());
            entity.setUserEntity(userEntity);
        }

        entity.setContractSubject(dto.getContractSubject());
        entity.setValidityStart(dto.getValidityStart());
        entity.setEffectStart(dto.getEffectStart());
        entity.setAddition(dto.getAddition());

        if (dto.getContractParty() != null) {
            SubjectEntity subjectEntity = new SubjectEntity();
            SubjectFactory.fillEntity(subjectEntity, dto.getContractParty());
            entity.setContractParty(subjectEntity);
        }

        if (dto.getType() != null) {
            ContractTypeEntity contractTypeEntity = new ContractTypeEntity();
            ContractTypeFactory.fillEntity(contractTypeEntity, dto.getType());
            entity.setType(contractTypeEntity);
        }

        entity.setEvidenceNo(dto.getEvidenceNo());

        if (dto.getContractState() != null) {
            EnumerationEntity enumerationEntity = new EnumerationEntity();
            EnumerationFactory.fillEntity(enumerationEntity, dto.getContractState());
            entity.setContractState(enumerationEntity);
        }

        entity.setPriceNoVat(dto.getPriceNoVat());
        entity.setMaturityInvoice(dto.getMaturityInvoice());

        if (dto.getOpportunityDto() != null) {
            OpportunityEntity opportunityEntity = new OpportunityEntity();
            OpportunityFactory.fillEntity(opportunityEntity, dto.getOpportunityDto());
            entity.setOpportunityEntity(opportunityEntity);
        }
        entity.setSigned(dto.getSigned());
        if (dto.getSendNotificationDaysBefore() != null) {
            entity.setSendNotificationDaysBefore(dto.getSendNotificationDaysBefore().getDays());
        }
        entity.setLastUpdate(LocalDateTime.now());
        if (dto.getAppCurrency() != null) {
            entity.setAppCurrency(dto.getAppCurrency().name());
        } else {
            entity.setAppCurrency(AppCurrency.CZK.name());
        }
        if (dto.getInternalType() != null) {
            entity.setContractInternalType(dto.getInternalType().name());
        }
    }
}
