package cz.bbn.cerberus.employeecontract.factory;

import cz.bbn.cerberus.contracttype.factory.ContractTypeFactory;
import cz.bbn.cerberus.contracttype.persistence.ContractTypeEntity;
import cz.bbn.cerberus.employee.factory.EmployeeFactory;
import cz.bbn.cerberus.employee.persistance.entity.EmployeeEntity;
import cz.bbn.cerberus.employeecontract.dto.EmployeeContractDto;
import cz.bbn.cerberus.employeecontract.persistance.EmployeeContractEntity;
import cz.bbn.cerberus.enumeration.factory.EnumerationFactory;
import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationEntity;
import cz.bbn.cerberus.subject.factory.SubjectFactory;
import cz.bbn.cerberus.subject.persistance.SubjectEntity;

public class EmployeeContractFactory {

    private EmployeeContractFactory() {
    }

    public static EmployeeContractDto fromEntity(EmployeeContractEntity entity) {
        EmployeeContractDto dto = new EmployeeContractDto();
        dto.setId(entity.getId());
        if (entity.getOwnCompany() != null) {
            dto.setOwnCompany(SubjectFactory.fromEntity(entity.getOwnCompany()));
        }
        if (entity.getEmployee() != null) {
            dto.setEmployee(EmployeeFactory.fromEntity(entity.getEmployee()));
        }
        if (entity.getType() != null) {
            dto.setType(ContractTypeFactory.fromEntity(entity.getType()));
        }
        dto.setLinkedContractId(entity.getLinkedContract());
        dto.setName(entity.getName());
        dto.setValidFrom(entity.getValidFrom());
        dto.setValidTo(entity.getValidTo());
        dto.setReminder(entity.getReminder());
        dto.setContractNumber(entity.getContractNumber());
        if (entity.getState() != null) {
            dto.setState(EnumerationFactory.fromEntity(entity.getState()));
        }
        dto.setDescription(entity.getDescription());
        dto.setCreationDate(entity.getCreationDate());
        dto.setSequence(entity.getSequence());
        dto.setSubsequence(entity.getSubsequence());
        dto.setDeleted(entity.getDeleted());
        dto.setArchived(entity.getArchived());
        return dto;
    }

    public static void fillEntity(EmployeeContractEntity entity, EmployeeContractDto dto) {
        entity.setId(dto.getId());
        if (dto.getOwnCompany() != null) {
            SubjectEntity subjectEntity = new SubjectEntity();
            SubjectFactory.fillEntity(subjectEntity, dto.getOwnCompany());
            entity.setOwnCompany(subjectEntity);
        }
        if (dto.getEmployee() != null) {
            EmployeeEntity employeeEntity = new EmployeeEntity();
            EmployeeFactory.fillEntity(employeeEntity, dto.getEmployee());
            entity.setEmployee(employeeEntity);
        }
        if (dto.getType() != null) {
            ContractTypeEntity typeEntity = new ContractTypeEntity();
            ContractTypeFactory.fillEntity(typeEntity, dto.getType());
            entity.setType(typeEntity);
        }
        entity.setLinkedContract(dto.getLinkedContractId());
        entity.setName(dto.getName());
        entity.setValidFrom(dto.getValidFrom());
        entity.setValidTo(dto.getValidTo());
        entity.setReminder(dto.getReminder());
        entity.setContractNumber(dto.getContractNumber());
        if (dto.getState() != null) {
            EnumerationEntity stateEntity = new EnumerationEntity();
            EnumerationFactory.fillEntity(stateEntity, dto.getState());
            entity.setState(stateEntity);
        }
        entity.setDescription(dto.getDescription());
        entity.setCreationDate(dto.getCreationDate());
        entity.setSequence(dto.getSequence());
        entity.setSubsequence(dto.getSubsequence());
        entity.setDeleted(dto.getDeleted());
        entity.setArchived(dto.getArchived());
    }
}
