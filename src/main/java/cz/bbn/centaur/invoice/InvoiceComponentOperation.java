package cz.bbn.cerberus.invoice;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.combobox.ComboBox;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.field.AppMoneyField;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.ContractService;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.document.DocumentObjectEnum;
import cz.bbn.cerberus.document.DocumentService;
import cz.bbn.cerberus.dph.DphService;
import cz.bbn.cerberus.dph.dto.DphDto;
import cz.bbn.cerberus.dph.factory.DphFactory;
import cz.bbn.cerberus.holiday.HolidayService;
import cz.bbn.cerberus.holiday.persistance.HolidayEntity;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.invoice.dto.InvoiceFilterDto;
import cz.bbn.cerberus.invoice.dto.InvoiceState;
import cz.bbn.cerberus.invoice.dto.InvoicingDto;
import cz.bbn.cerberus.invoice.dto.InvoicingPeriod;
import cz.bbn.cerberus.invoice.ui.InvoicingDetailView;
import cz.bbn.cerberus.invoice.ui.component.InvoiceFilterComponent;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.pohoda.PohodaService;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;
import jakarta.xml.bind.JAXBException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.money.CurrencyUnit;
import javax.money.Monetary;
import javax.money.MonetaryAmount;
import javax.money.MonetaryOperator;
import javax.xml.datatype.DatatypeConfigurationException;
import java.io.IOException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

@Component
@Slf4j
public class InvoiceComponentOperation {

    private final InvoicingService invoicingService;
    private final DocumentService documentService;
    private final AppEnv appEnv;
    private final PohodaService pohodaService;
    private final ListService listService;
    private final DphService dphService;
    private final ContractService contractService;
    private final HolidayService holidayService;

    public InvoiceComponentOperation(InvoicingService invoicingService, DocumentService documentService,
                                     AppEnv appEnv, PohodaService pohodaService, ListService listService,
                                     DphService dphService, ContractService contractService,
                                     HolidayService holidayService) {
        this.invoicingService = invoicingService;
        this.documentService = documentService;
        this.appEnv = appEnv;
        this.pohodaService = pohodaService;
        this.listService = listService;
        this.dphService = dphService;
        this.contractService = contractService;
        this.holidayService = holidayService;
    }

    public ItemsAction<InvoiceDto> getItemsAction(List<ContractDto> contractDtoList) {
        return (query, sortOrder) -> invoicingService.getInvoiceDtoByContractPage(contractDtoList, query, sortOrder);
    }

    public ItemsAction<InvoiceDto> getItemsAction(InvoiceFilterComponent filter) {
        return ((query, sortOrder) -> {
            InvoiceFilterDto invoiceFilterDto = filter.getInvoiceFilterDto();
            invoiceFilterDto.setPage(query.getPage());
            invoiceFilterDto.setOrderList(sortOrder);
            invoiceFilterDto.setSize(query.getPageSize());
            return invoicingService.getInvoiceDtoPage(invoiceFilterDto);
        });
    }

    public DeleteAction getDeleteAction() {
        return id -> {
            try {
                invoicingService.softDelete(id);
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException e) {
                log.error(e.getMessage(), e);
                ErrorNotification.show(Transl.get(e.getMessage()), appEnv);
            }
        };
    }

    public SaveAction<InvoicingDto> getSaveAction() {
        return (dto, originalDto) -> {
            InvoiceState state = Boolean.TRUE.equals(dto.isToBeInvoicedState())
                    ? InvoiceState.TO_BE_INVOICED : InvoiceState.NEW;
            List<InvoiceDto> invoiceDtoList = new ArrayList<>();
            if (dto.getInvoicingPeriod() == null) {
                LocalDate invoicingDate = dto.getInvoicingStart();
                invoicingDate = getCalculatedDate(invoicingDate);
                InvoiceDto invoiceDto = getInitiatedInvoiceDto(dto, invoicingDate, state);
                invoiceDtoList.add(invoiceDto);
            } else if (InvoicingPeriod.YEARLY == dto.getInvoicingPeriod()) {
                invoiceDtoList = generateInvoiceDtoList(dto, 12, state);
            } else if (InvoicingPeriod.HALF_YEARLY == dto.getInvoicingPeriod()) {
                invoiceDtoList = generateInvoiceDtoList(dto, 6, state);
            } else if (InvoicingPeriod.QUARTERLY == dto.getInvoicingPeriod()) {
                invoiceDtoList = generateInvoiceDtoList(dto, 3, state);
            } else {
                invoiceDtoList = generateInvoiceDtoList(dto, 1, state);
            }
            Long lastId = invoicingService.saveAll(invoiceDtoList, dto.getAreaTechnologySignDtoSet());

            if (invoiceDtoList.size() == 1) {
                UI.getCurrent().navigate(InvoicingDetailView.ROUTE.concat("/").concat(String.valueOf(lastId)));
            }
        };
    }

    public List<String> getDocumentNameListByContract(String contractId) {
        return documentService.findDocumentNameListByContract(contractId, DocumentObjectEnum.CONTRACT);
    }

    public SaveAction<InvoiceDto> getSingleItemSaveAction() {
        return (dto, originalDto) -> {
            try {
                invoicingService.saveInvoiceDto(dto);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    private List<InvoiceDto> getInvoiceDtoListByMonths(int monthPeriod, InvoicingDto dto, LocalDate invoicingDay) {
        List<InvoiceDto> invoiceList = new ArrayList<>();
        while (invoicingDay.isBefore(dto.getInvoicingEnd())) {
            InvoiceDto invoiceDto;
            if (dto.isToBeInvoicedState()) {
                invoiceDto = getInitiatedInvoiceDto(dto, getCalculatedDate(invoicingDay), InvoiceState.TO_BE_INVOICED);
            } else {
                invoiceDto = getInitiatedInvoiceDto(dto, getCalculatedDate(invoicingDay), InvoiceState.NEW);
            }
            invoiceList.add(invoiceDto);
            invoicingDay = invoicingDay.plusMonths(monthPeriod);
        }
        return invoiceList;
    }

    private InvoiceDto getInitiatedInvoiceDto(InvoicingDto invoicingDto, LocalDate invoicingDay,
                                              InvoiceState invoiceState) {
        InvoiceDto invoiceDto = new InvoiceDto();
        invoiceDto.setDeleted(false);
        invoiceDto.setInvoicingDate(invoicingDay);
        invoiceDto.setState(invoiceState);
        invoiceDto.setContractDto(invoicingDto.getContract());
        invoiceDto.setTransferProtocol(invoicingDto.isTransferProtocolPresent());
        invoiceDto.setPriceNoVat(invoicingDto.getPriceNoVat());
        invoiceDto.setDphDto(invoicingDto.getDphDto());
        invoiceDto.setPriceTotal(invoicingDto.getPriceTotal());
        invoiceDto.setAppCurrency(invoicingDto.getAppCurrency());
        invoiceDto.setReminderWorkDays(invoicingDto.getReminderWorkDays());
        invoiceDto.setDaysToPay(invoicingDto.getMaturityInvoice());
        invoiceDto.setDescription(invoicingDto.getDescription());
        invoiceDto.setAddressee(invoicingDto.getAddressee());
        invoiceDto.setUserDto(SecurityUtils.getCurrentUserDto());
        invoiceDto.setAreaTechnologySignDtoSet(invoicingDto.getAreaTechnologySignDtoSet());
        return invoiceDto;
    }

    private LocalDate getCalculatedDate(LocalDate invoicingDate) {
        List<LocalDate> holidayList =
                holidayService.findAll().stream().map(HolidayEntity::getDate).toList();

        if (invoicingDate.getDayOfWeek().getValue() != 7 && invoicingDate.getDayOfWeek().getValue() != 6
                && !holidayList.contains(invoicingDate)) {
            return invoicingDate;
        }

        int fInt = 0;
        boolean fIsOut = false;


        for (int i = 0; i < 10; i++) {
            LocalDate tempDateF = invoicingDate.plusDays(i);
            if (tempDateF.getDayOfWeek().getValue() != 7 && tempDateF.getDayOfWeek().getValue() != 6
                    && !holidayList.contains(tempDateF)) {
                break;
            }
            if (tempDateF.getMonth() != invoicingDate.getMonth()) {
                fIsOut = true;
                break;
            }
            fInt++;
        }

        int bInt = 0;
        boolean bIsOut = false;

        for (int i = 0; i < 10; i++) {
            LocalDate tempDateB = invoicingDate.minusDays(i);
            if (tempDateB.getDayOfWeek().getValue() != 7 && tempDateB.getDayOfWeek().getValue() != 6
                    && !holidayList.contains(tempDateB)) {
                break;
            }
            if (tempDateB.getMonth() != invoicingDate.getMonth()) {
                bIsOut = true;
                break;
            }
            bInt++;
        }

        if (fIsOut) {
            return invoicingDate.minusDays(bInt);
        }
        if (bIsOut) {
            return invoicingDate.plusDays(fInt);
        }
        if (fInt > bInt) {
            return invoicingDate.minusDays(bInt);
        }
        return invoicingDate.plusDays(fInt);

    }

    private List<InvoiceDto> generateInvoiceDtoList(InvoicingDto dto, int period, InvoiceState state) {
        LocalDate tempDate = LocalDate.of(dto.getInvoicingStart().getYear(), dto.getInvoicingStart().getMonth(),
                dto.getInvoicingDay());
        if (tempDate.isBefore(dto.getInvoicingStart()) && tempDate.plusMonths(period).isBefore(dto.getInvoicingEnd())) {
            tempDate = tempDate.plusMonths(period);
            return getInvoiceDtoListByMonths(period, dto, tempDate);
        }
        if ((tempDate.isAfter(dto.getInvoicingStart()) || tempDate.isEqual(dto.getInvoicingStart()))
                && tempDate.plusMonths(period).isBefore(dto.getInvoicingEnd())) {
            return getInvoiceDtoListByMonths(period, dto, tempDate);
        }

        InvoiceDto invoiceDto = getInitiatedInvoiceDto(dto, getCalculatedDate(tempDate), state);
        return new ArrayList<>(List.of(invoiceDto));
    }

    public SaveAction<InvoiceDto> saveInvoiceToPohoda() {
        return (dto, originalDto) -> {
            try {
                pohodaService.createInvoice(dto);
            } catch (RuntimeException | IOException | JAXBException | DatatypeConfigurationException e) {
                log.error("exception", e);
                ErrorNotification.show(Transl.get("Exception occurred"), appEnv);
            }
        };
    }

    public String getObjectName(DocumentObjectEnum documentObjectEnum) {
        return switch (documentObjectEnum) {
            case PROJECT -> DomainEnum.PROJECT_DOMAIN_NAME.getValue();
            case CONTACT_PERSON -> DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue();
            case SUBJECT -> DomainEnum.SUBJECT_DOMAIN_NAME.getValue();
            case CONTRACT -> DomainEnum.CONTRACT_DOMAIN_NAME.getValue();
            default -> "";
        };

    }

    public String getContractName(String contractId) {
        return listService.getNameByIdAndObjectType(contractId, DomainEnum.CONTRACT_DOMAIN_NAME.getValue());
    }

    public ContractDto getContract(String contractId) {
        return listService.getContractMap().get(contractId);
    }

    public List<DphDto> getDphList() {
        return ConvertEntities.fromEntities(dphService.getAllAllowed(), DphFactory::fromEntity);
    }

    public List<ContractDto> getContractListBySubject(SubjectDto subjectDto) {
        return contractService.getContractListBySubject(subjectDto.getId());
    }

    public DphDto getDefaultDph() {
        return dphService.getDefaultDph();
    }


    public Double getFullPriceValue(AppMoneyField priceNoVat, ComboBox<DphDto> vat) {
        if (priceNoVat.getValue() != null && vat.getValue() != null) {
            return getFullPrice(priceNoVat.getValue().doubleValue(),
                    vat.getValue().getValue());
        }
        if (priceNoVat.getValue() != null) {
            return priceNoVat.getValue().doubleValue();
        }
        return 0d;
    }

    public Double getFullPrice(Double priceNoVat, double vat) {
        CurrencyUnit czk = Monetary.getCurrency("CZK");
        if (priceNoVat == null || priceNoVat.isNaN()) {
            priceNoVat = 0.0;
        }
        MonetaryAmount noVatMon = Monetary.getDefaultAmountFactory().setCurrency(czk).setNumber(priceNoVat).create();
        MonetaryAmount fullPrice = noVatMon.multiply(1 + vat);
        MonetaryOperator r = Monetary.getRounding(czk);
        fullPrice = fullPrice.with(r);
        return fullPrice.getNumber().doubleValueExact();
    }
}
