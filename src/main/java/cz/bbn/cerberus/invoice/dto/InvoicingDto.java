package cz.bbn.cerberus.invoice.dto;

import cz.bbn.cerberus.areatechnologysign.AreaTechnologySignDto;
import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.dph.dto.DphDto;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Set;

@Getter
@Setter
public class InvoicingDto {

    private boolean transferProtocolPresent;
    private InvoicingPeriod invoicingPeriod;
    private LocalDate invoicingEnd;
    private LocalDate invoicingStart;
    private Integer invoicingDay;
    private ContractDto contract;

    private SubjectDto subject;
    private Double priceNoVat;
    private DphDto dphDto;
    private Double priceTotal;
    private AppCurrency appCurrency;
    private Integer reminderWorkDays;
    private Integer maturityInvoice;

    private String description;
    private String addressee;

    private boolean toBeInvoicedState;

    private Set<AreaTechnologySignDto> areaTechnologySignDtoSet;


    public BigDecimal getPriceNoVatBigDecimal() {
        if (priceNoVat != null) {
            return BigDecimal.valueOf(priceNoVat);
        }
        return BigDecimal.ZERO;
    }

    public void setPriceNoVatBigDecimal(BigDecimal priceNoVatBigDecimal) {
        if (priceNoVatBigDecimal != null) {
            this.priceNoVat = priceNoVatBigDecimal.doubleValue();
        }
    }
}
