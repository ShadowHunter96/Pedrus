package cz.bbn.cerberus.suppliertype.factory;

import cz.bbn.cerberus.suppliertype.dto.SupplierTypeDto;
import cz.bbn.cerberus.suppliertype.persistance.SupplierTypeEntity;

public class SupplierTypeFactory {

    private SupplierTypeFactory() {
    }


    public static SupplierTypeDto fromEntity(SupplierTypeEntity entity) {
        SupplierTypeDto dto = new SupplierTypeDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setAllowed(Boolean.TRUE.equals(entity.getAllowed()));
        return dto;
    }

    public static void fillEntity(SupplierTypeEntity entity, SupplierTypeDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setAllowed(Boolean.TRUE.equals(dto.getAllowed()));
    }
}
