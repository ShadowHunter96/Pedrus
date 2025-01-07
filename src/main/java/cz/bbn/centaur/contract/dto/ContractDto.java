package cz.bbn.cerberus.contract.dto;

import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Getter
@Setter
@ToString
public class ContractDto implements Serializable {

    private String id;
    private String name;
    private String description;
    private Boolean deleted;
    private LocalDate endContract;

    private SubjectDto subjectDto;
    private ContractDto connectedContract;

    private UserDto userDto;

    private String contractSubject;
    private LocalDate validityStart;
    private LocalDate effectStart;
    private Boolean addition;
    private SubjectDto contractParty;
    private ContractTypeDto type;
    private String evidenceNo;

    private EnumerationDto contractState;
    private BigDecimal priceNoVat;
    private Integer maturityInvoice;
    private OpportunityDto opportunityDto;
    private Signed signed;
    private ContractEndingDays sendNotificationDaysBefore;

    private LocalDateTime lastUpdate;

    private AppCurrency appCurrency;

    private ContractInternalType internalType;
}
