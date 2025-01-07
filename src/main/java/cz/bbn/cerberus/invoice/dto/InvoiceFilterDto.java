package cz.bbn.cerberus.invoice.dto;

import cz.bbn.cerberus.commons.component.ui.enums.FilterBoolean;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.time.LocalDate;
import java.util.List;

@Getter
@Setter
public class InvoiceFilterDto {

    private LocalDate invoicingDateStart;
    private LocalDate invoicingDateEnd;
    private LocalDate issueDateStart;
    private LocalDate issueDateEnd;
    private FilterBoolean payed;
    private String contractId;
    private UserDto userDto;
    private String subjectId;
    private boolean showDeleted;
    private boolean onlyEditPermission;
    private FilterBoolean transferProtocol;
    private InvoiceState invoiceState;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
