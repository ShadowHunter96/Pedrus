package cz.bbn.cerberus.offer.dto;


import cz.bbn.cerberus.commons.enums.AppCurrency;
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
public class OfferDto implements Serializable {
    private String id;
    private String name;

    private OpportunityDto opportunityDto;
    private BigDecimal priceWithoutVat;
    private AppCurrency appCurrency;
    private LocalDate offerDate;
    private LocalDate validityDate;
    private String customerReference;
    private String marketUrl;
    private String svnUrl;
    private UserDto processedByUserDto;
    private SubjectDto subjectDto;
    private SubjectDto ownOrganizationSubjectDto;
    private Boolean assurance;
    private BigDecimal priceAssurance;
    private Boolean sent;
    private Boolean explanation;
    private OfferState state;
    private Boolean deleted;
    private LocalDateTime lastUpdate;
    private Integer sequence;

}
