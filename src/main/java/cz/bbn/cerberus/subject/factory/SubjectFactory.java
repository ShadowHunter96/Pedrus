package cz.bbn.cerberus.subject.factory;

import cz.bbn.cerberus.adis.AdisReliable;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.subject.persistance.SubjectEntity;
import cz.bbn.cerberus.suppliertype.factory.SupplierTypeFactory;
import cz.bbn.cerberus.suppliertype.persistance.SupplierTypeEntity;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;

import java.math.BigDecimal;

public class SubjectFactory {

    private SubjectFactory() {
    }

    public static SubjectDto fromEntity(SubjectEntity entity) {
        SubjectDto dto = new SubjectDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setDescription(entity.getDescription());
        dto.setUrl(entity.getUrl());
        dto.setCourt(entity.getCourt());
        dto.setFileNumber(entity.getFileNumber());
        dto.setRegister(entity.getRegister());
        dto.setIco(entity.getIco());
        dto.setCompanyName(entity.getCompanyName());
        dto.setLawForm(entity.getLawForm());
        dto.setAddress(entity.getAddress());
        dto.setEnlistDate(entity.getEnlistDate());
        dto.setCapital(entity.getCapital());
        dto.setCompanions(entity.getCompanions());
        dto.setDic(entity.getDic());
        dto.setReliable(AdisReliable.fromString(entity.getReliable()));
        dto.setUnreliableFrom(entity.getUnreliableFrom());
        dto.setStandardAccount(entity.getStandardAccount());
        dto.setNonStandardAccount(entity.getNonStandardAccount());
        if (entity.getUserEntity() != null) {
            dto.setUserDto(UserFactory.fromEntity(entity.getUserEntity()));
        }
        dto.setLocalSubject(entity.getLocalSubject());
        dto.setCustomer(entity.getCustomer());
        dto.setSupplier(entity.getSupplier());
        dto.setOwnCompany(entity.getOwnCompany());
        if (entity.getSupplierType() != null) {
            dto.setSupplierType(SupplierTypeFactory.fromEntity(entity.getSupplierType()));
        }
        if (entity.getCapital() != null && !"".equals(entity.getCapital().trim().replaceAll("[^\\d.]", ""))) {
            dto.setCapitalDecimal(BigDecimal.valueOf(Double.parseDouble(entity.getCapital().trim().replace(",", ".")
                    .replaceAll("[^\\d.]", ""))));
        }
        dto.setDeleted(entity.getDeleted());
        dto.setAppCurrency(entity.getAppCurrency());
        dto.setLastUpdateFromAres(entity.getLastUpdateFromAres());
        return dto;
    }

    public static void fillEntity(SubjectEntity entity, SubjectDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setDescription(dto.getDescription());
        entity.setUrl(dto.getUrl());
        entity.setCourt(dto.getCourt());
        entity.setFileNumber(dto.getFileNumber());
        entity.setRegister(dto.getRegister());
        entity.setIco(dto.getIco());
        entity.setCompanyName(dto.getCompanyName());
        entity.setLawForm(dto.getLawForm());
        entity.setAddress(dto.getAddress());
        entity.setEnlistDate(dto.getEnlistDate());
        entity.setCapital(dto.getCapital());
        entity.setCompanions(dto.getCompanions());
        entity.setDic(dto.getDic());
        if (dto.getReliable() != null) {
            entity.setReliable(dto.getReliable().name());
        }
        entity.setUnreliableFrom(dto.getUnreliableFrom());
        entity.setStandardAccount(dto.getStandardAccount());
        entity.setNonStandardAccount(dto.getNonStandardAccount());
        if (dto.getUserDto() != null) {
            UserEntity userEntity = new UserEntity();
            UserFactory.fillEntity(userEntity, dto.getUserDto());
            entity.setUserEntity(userEntity);
        }
        entity.setLocalSubject(dto.getLocalSubject());
        entity.setCustomer(dto.getCustomer());
        entity.setSupplier(dto.getSupplier());
        entity.setOwnCompany(dto.getOwnCompany());
        entity.setDeleted(Boolean.TRUE.equals(dto.getDeleted()));
        if (dto.getSupplierType() != null) {
            SupplierTypeEntity supplierTypeEntity = new SupplierTypeEntity();
            SupplierTypeFactory.fillEntity(supplierTypeEntity, dto.getSupplierType());
            entity.setSupplierType(supplierTypeEntity);
        }
        if (!Boolean.TRUE.equals(dto.getLocalSubject()) && dto.getCapitalDecimal() != null) {
            entity.setCapital(dto.getCapitalDecimal().toString());
        }
        entity.setAppCurrency(dto.getAppCurrency());
        entity.setLastUpdateFromAres(dto.getLastUpdateFromAres());
    }
}
