package cz.bbn.cerberus.asset.factory;

import cz.bbn.cerberus.asset.dto.AssetDto;
import cz.bbn.cerberus.asset.dto.AssetSimpleDto;
import cz.bbn.cerberus.asset.persistance.entity.AssetEntity;
import cz.bbn.cerberus.asset.persistance.entity.AssetSimpleEntity;
import cz.bbn.cerberus.assetposition.factory.AssetPositionFactory;
import cz.bbn.cerberus.assetposition.persistance.AssetPositionEntity;
import cz.bbn.cerberus.employee.factory.EmployeeFactory;
import cz.bbn.cerberus.employee.persistance.entity.EmployeeEntity;
import cz.bbn.cerberus.enumeration.factory.EnumerationFactory;
import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationEntity;
import cz.bbn.cerberus.subject.factory.SubjectFactory;
import cz.bbn.cerberus.subject.persistance.SubjectEntity;

public class AssetFactory {

    private AssetFactory() {
    }

    public static AssetSimpleDto fromEntity(AssetSimpleEntity entity) {
        AssetSimpleDto assetSimpleDto = new AssetSimpleDto();
        assetSimpleDto.setId(entity.getId());
        assetSimpleDto.setName(entity.getName());
        assetSimpleDto.setSerialNumber(entity.getSerialNumber());
        assetSimpleDto.setResponsiblePerson(entity.getResponsiblePerson());
        assetSimpleDto.setPrice(entity.getPrice());
        assetSimpleDto.setBuyDate(entity.getBuyDate());
        assetSimpleDto.setType(entity.getType());
        assetSimpleDto.setDeleted(entity.getDeleted());
        return assetSimpleDto;
    }

    public static void fillEntity(AssetSimpleEntity entity, AssetSimpleDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setSerialNumber(dto.getSerialNumber());
        entity.setResponsiblePerson(dto.getResponsiblePerson());
        entity.setPrice(dto.getPrice());
        entity.setBuyDate(dto.getBuyDate());
        entity.setType(dto.getType());
    }

    public static AssetDto fromEntity(AssetEntity entity) {
        AssetDto dto = new AssetDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setDescription(entity.getDescription());
        dto.setSerialNumber(entity.getSerialNumber());
        dto.setType(entity.getType());
        dto.setBuyDate(entity.getBuyDate());
        dto.setPrice(entity.getPrice());
        dto.setRemovalDate(entity.getRemovalDate());
        dto.setQuaranteeDate(entity.getQuaranteeDate());
        dto.setDestination(entity.getDestination());
        dto.setInventoryDate(entity.getInventoryDate());
        dto.setDeleted(entity.getDeleted());
        if (entity.getAssetPositionEntity() != null) {
            dto.setAssetPositionDto(AssetPositionFactory.fromEntity(entity.getAssetPositionEntity()));
        }
        dto.setLongitude(entity.getLongitude());
        dto.setLatitude(entity.getLatitude());
        dto.setPohodaId(entity.getPohodaId());
        if (entity.getOurCompany() != null) {
            dto.setOurCompany(SubjectFactory.fromEntity(entity.getOurCompany()));
        }
        dto.setProcurement(entity.getProcurement());
        if (entity.getResponsiblePerson() != null) {
            dto.setResponsiblePerson(EmployeeFactory.fromEntity(entity.getResponsiblePerson()));
        }
        dto.setDepreciation(entity.getDepreciation());
        dto.setLocation(entity.getLocation());
        if (entity.getCategory() != null) {
            dto.setCategory(EnumerationFactory.fromEntity(entity.getCategory()));
        }
        dto.setState(entity.getState());
        dto.setAppCurrency(entity.getAppCurrency());


        return dto;
    }

    public static void fillEntity(AssetEntity entity, AssetDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setDescription(dto.getDescription());
        entity.setSerialNumber(dto.getSerialNumber());
        entity.setType(dto.getType());
        entity.setBuyDate(dto.getBuyDate());
        entity.setPrice(dto.getPrice());
        entity.setRemovalDate(dto.getRemovalDate());
        entity.setQuaranteeDate(dto.getQuaranteeDate());
        entity.setDestination(dto.getDestination());
        entity.setInventoryDate(dto.getInventoryDate());
        entity.setDeleted(false);

        if (dto.getAssetPositionDto() != null && !"own".equalsIgnoreCase(dto.getAssetPositionDto().getId())) {
            AssetPositionEntity assetPositionEntity = new AssetPositionEntity();
            AssetPositionFactory.fillEntity(assetPositionEntity, dto.getAssetPositionDto());
            entity.setAssetPositionEntity(assetPositionEntity);
        } else {
            entity.setAssetPositionEntity(null);
        }

        entity.setLongitude(dto.getLongitude());
        entity.setLatitude(dto.getLatitude());

        entity.setPohodaId(dto.getPohodaId());

        if (dto.getOurCompany() != null) {
            SubjectEntity subjectEntity = new SubjectEntity();
            SubjectFactory.fillEntity(subjectEntity, dto.getOurCompany());
            entity.setOurCompany(subjectEntity);
        }

        entity.setProcurement(dto.getProcurement());

        if (dto.getResponsiblePerson() != null) {
            EmployeeEntity employeeEntity = new EmployeeEntity();
            EmployeeFactory.fillEntity(employeeEntity, dto.getResponsiblePerson());
            entity.setResponsiblePerson(employeeEntity);
        }

        entity.setDepreciation(dto.getDepreciation());
        entity.setLocation(dto.getLocation());

        if (dto.getCategory() != null) {
            EnumerationEntity enumerationEntity = new EnumerationEntity();
            EnumerationFactory.fillEntity(enumerationEntity, dto.getCategory());
            entity.setCategory(enumerationEntity);
        }

        entity.setAppCurrency(dto.getAppCurrency());

        entity.setState(dto.getState());
    }
}
