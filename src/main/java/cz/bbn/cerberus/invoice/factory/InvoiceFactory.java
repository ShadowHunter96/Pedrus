package cz.bbn.cerberus.invoice.factory;

import cz.bbn.cerberus.contract.factory.ContractFactory;
import cz.bbn.cerberus.contract.persistence.entity.ContractEntity;
import cz.bbn.cerberus.dph.factory.DphFactory;
import cz.bbn.cerberus.dph.persistance.entity.DphEntity;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.invoice.dto.InvoiceState;
import cz.bbn.cerberus.invoice.persistance.entity.InvoiceEntity;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;

public class InvoiceFactory {

    private InvoiceFactory() {
    }

    public static InvoiceDto fromEntity(InvoiceEntity entity) {
        InvoiceDto dto = new InvoiceDto();
        dto.setId(entity.getId());
        dto.setDescription(entity.getDescription());
        dto.setIssueDate(entity.getIssueDate());
        dto.setContractDto(ContractFactory.fromEntity(entity.getContractEntity()));
        dto.setDeleted(entity.getDeleted());
        dto.setTransferProtocol(entity.getTransferProtocol());
        dto.setDocumentName(entity.getDocumentName());
        dto.setCreatedInPohoda(entity.getCreatedInPohoda());
        dto.setPriceNoVat(entity.getPriceNoVat());
        if (entity.getDph() != null) {
            dto.setDphDto(DphFactory.fromEntity(entity.getDph()));
        }
        dto.setPriceTotal(entity.getPriceTotal());
        dto.setTaxDate(entity.getTaxDate());
        dto.setDaysToPay(entity.getDaysToPay());
        dto.setPaymentDate(entity.getPaymentDate());
        dto.setInvoiceNo(entity.getInvoiceNo());
        dto.setAddressee(entity.getAddressee());
        dto.setReminderWorkDays(entity.getReminderWorkDays());
        dto.setInvoicingDate(entity.getInvoicingDate());
        dto.setAppCurrency(entity.getAppCurrency());
        if (entity.getUserEntity() != null) {
            dto.setUserDto(UserFactory.fromEntity(entity.getUserEntity()));
        }
        dto.setState(InvoiceState.getState(entity.getState()));
        dto.setStringId(entity.getStringId());
        return dto;
    }

    public static void fillEntity(InvoiceEntity entity, InvoiceDto dto) {
        entity.setId(dto.getId());
        entity.setDescription(dto.getDescription());
        entity.setIssueDate(dto.getIssueDate());

        ContractEntity contractEntity = new ContractEntity();
        ContractFactory.fillEntity(contractEntity, dto.getContractDto());
        entity.setContractEntity(contractEntity);

        entity.setDeleted(dto.getDeleted());
        entity.setTransferProtocol(dto.getTransferProtocol());
        entity.setDocumentName(dto.getDocumentName());
        if (dto.getCreatedInPohoda() != null) {
            entity.setCreatedInPohoda(dto.getCreatedInPohoda());
        } else {
            entity.setCreatedInPohoda(false);
        }
        entity.setPriceNoVat(dto.getPriceNoVat());
        if (dto.getDphDto() != null) {
            DphEntity dphEntity = new DphEntity();
            DphFactory.fillEntity(dphEntity, dto.getDphDto());
            entity.setDph(dphEntity);
        }
        entity.setPriceTotal(dto.getPriceTotal());
        entity.setTaxDate(dto.getTaxDate());
        entity.setDaysToPay(dto.getDaysToPay());
        entity.setPaymentDate(dto.getPaymentDate());
        entity.setInvoiceNo(dto.getInvoiceNo());
        entity.setAddressee(dto.getAddressee());
        entity.setReminderWorkDays(dto.getReminderWorkDays());
        entity.setInvoicingDate(dto.getInvoicingDate());
        entity.setAppCurrency(dto.getAppCurrency());
        if (dto.getUserDto() != null) {
            UserEntity userEntity = new UserEntity();
            UserFactory.fillEntity(userEntity, dto.getUserDto());
            entity.setUserEntity(userEntity);
        }
        if (dto.getState() != null) {
            entity.setState(dto.getState().name());
        }
        entity.setStringId(dto.getStringId());
    }
}
