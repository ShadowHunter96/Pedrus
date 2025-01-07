package cz.bbn.cerberus.opportunity.dto;


import cz.bbn.cerberus.area.dto.AreaDto;
import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.technology.dto.TechnologyDto;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Set;

@Getter
@Setter
@ToString
@Slf4j
public class OpportunityDto implements Serializable {

    private String id;
    private String name;
    private String description;
    private Integer progress;
    private Integer successChance;
    private SubjectDto subject;
    private UserDto user;

    private SubjectDto primarySupplier;

    private BigDecimal volume;

    private AppCurrency appCurrency;

    private LocalDate startDate;

    private OpportunityState state;

    private SubjectDto winnerSubject;

    private String winningTechnology;

    private Boolean deleted;
    private LocalDateTime createDate;

    private LocalDate dateOfFulfilment;

    private Set<AreaDto> araAreaDtoSet;
    private Set<TechnologyDto> technologyDtoSet;

    private BigDecimal expectedCosts;

    private BigDecimal expectedReturn;

}
