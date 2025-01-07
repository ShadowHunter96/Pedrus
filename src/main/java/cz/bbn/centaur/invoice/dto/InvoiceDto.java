package cz.bbn.cerberus.invoice.dto;

import cz.bbn.cerberus.areatechnologysign.AreaTechnologySignDto;
import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.dph.dto.DphDto;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Set;

@Getter
@Setter
@ToString
public class InvoiceDto implements Serializable {

    private Long id;
    private String description;
    private LocalDate invoicingDate;
    private LocalDate issueDate;
    private Boolean transferProtocol;
    private String documentName;
    private ContractDto contractDto;
    private Boolean deleted;

    private Boolean createdInPohoda;

    private Double priceNoVat;
    private DphDto dphDto;
    private Double priceTotal;
    private LocalDate taxDate;
    private Integer daysToPay;
    private LocalDate paymentDate;
    private String invoiceNo;
    private String addressee;
    private Integer reminderWorkDays;
    private AppCurrency appCurrency;
    private UserDto userDto;

    private InvoiceState state;
    private String expenseInvoices;
    private String stringId;

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
