package cz.bbn.cerberus.contracttype.factory;

import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.contracttype.persistence.ContractTypeEntity;

public class ContractTypeFactory {

    private ContractTypeFactory() {
    }

    public static ContractTypeDto fromEntity(ContractTypeEntity entity) {
        ContractTypeDto dto = new ContractTypeDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setDescription(entity.getDescription());
        dto.setAllowed(entity.getAllowed());
        dto.setConnectionRequired(entity.getConnectionRequired());
        dto.setSales(entity.getSales());
        dto.setOperational(entity.getOperational());
        dto.setSupplierCo(entity.getSupplierCo());
        dto.setEmployeeCo(entity.getEmployeeCo());
        return dto;
    }

    public static void fillEntity(ContractTypeEntity entity, ContractTypeDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setDescription(dto.getDescription());
        entity.setAllowed(dto.getAllowed());
        entity.setConnectionRequired(dto.getConnectionRequired());
        entity.setSales(dto.getSales());
        entity.setOperational(dto.getOperational());
        entity.setSupplierCo(dto.getSupplierCo());
        entity.setEmployeeCo(dto.getEmployeeCo());
    }
}
