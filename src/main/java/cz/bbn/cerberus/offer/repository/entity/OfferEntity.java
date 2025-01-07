package cz.bbn.cerberus.offer.repository.entity;

import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.offer.dto.OfferState;
import cz.bbn.cerberus.opportunity.persistance.entity.OpportunityEntity;
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

@Getter
@Setter
@Entity
@Table(name = "offer", schema = "sales")
public class OfferEntity {

    @Id
    private String id;

    private String name;

    @OneToOne
    @JoinColumn(name = "opportunity_id", referencedColumnName = "id")
    private OpportunityEntity opportunityEntity;

    @Column(name = "price_without_vat")
    private BigDecimal priceWithoutVat;

    @Enumerated(EnumType.STRING)
    @Column(name = "currency")
    private AppCurrency appCurrency;

    @Column(name = "offer_date")
    private LocalDate offerDate;

    @Column(name = "validity_date")
    private LocalDate validityDate;

    @Column(name = "customer_reference")
    private String customerReference;

    @Column(name = "market_url")
    private String marketUrl;

    @Column(name = "svn_url")
    private String svnUrl;

    @OneToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserEntity processedByUserEntity;

    @OneToOne
    @JoinColumn(name = "subject_id", referencedColumnName = "id")
    private SubjectEntity subjectEntity;

    @OneToOne
    @JoinColumn(name = "own_organization_id", referencedColumnName = "id")
    private SubjectEntity ownOrganizationSubjectEntity;

    private Boolean assurance;

    @Column(name = "price_assurance")
    private BigDecimal priceAssurance;

    private Boolean sent;

    private Boolean explanation;

    @Enumerated(EnumType.STRING)
    private OfferState state;

    private Boolean deleted;

    @Column(name = "last_update")
    private LocalDateTime lastUpdate;

    private Integer sequence;

}
