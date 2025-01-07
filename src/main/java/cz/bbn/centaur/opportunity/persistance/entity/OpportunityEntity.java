package cz.bbn.cerberus.opportunity.persistance.entity;

import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.opportunity.dto.OpportunityState;
import cz.bbn.cerberus.subject.persistance.SubjectEntity;
import cz.bbn.cerberus.user.persistance.UserEntity;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Entity
@Table(name = "opportunity", schema = "sales")
@Getter
@Setter
public class OpportunityEntity {

    @Id
    private String id;

    private String name;
    private String description;

    @OneToOne
    @JoinColumn(name = "subject_id", referencedColumnName = "id")
    private SubjectEntity subject;

    @OneToOne
    @JoinColumn(name = "primary_supplier_id", referencedColumnName = "id")
    private SubjectEntity primarySupplier;

    private BigDecimal volume;

    @Enumerated(EnumType.STRING)
    @Column(name = "currency")
    private AppCurrency appCurrency;

    @Column(name = "realisation_date")
    private LocalDate startDate;

    @Enumerated(EnumType.STRING)
    @Column(name = "state")
    private OpportunityState state;

    @OneToOne
    @JoinColumn(name = "winner_subject_id", referencedColumnName = "id")
    private SubjectEntity winnerSubject;

    @Column(name = "winning_tech")
    private String winningTechnology;

    @OneToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserEntity userEntity;

    private Integer progress;

    @Column(name = "success_chance")
    private Integer successChance;

    private Boolean deleted;

    @Column(name = "create_date")
    private LocalDateTime createDate;

    @Column(name = "fulfilment_date")
    private LocalDate dateOfFulfilment;

    @Column(name = "expected_costs")
    private BigDecimal expectedCosts;

    @Column(name = "expected_return")
    private BigDecimal expectedReturn;

}
