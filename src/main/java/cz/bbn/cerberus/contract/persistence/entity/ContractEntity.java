package cz.bbn.cerberus.contract.persistence.entity;

import cz.bbn.cerberus.areatechnologysign.persistance.AreaTechnologySignEntity;
import cz.bbn.cerberus.contract.dto.Signed;
import cz.bbn.cerberus.contracttype.persistence.ContractTypeEntity;
import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationEntity;
import cz.bbn.cerberus.opportunity.persistance.entity.OpportunityEntity;
import cz.bbn.cerberus.subject.persistance.SubjectEntity;
import cz.bbn.cerberus.user.persistance.UserEntity;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Set;

@Entity
@Getter
@Setter
@Table(name = "contract", schema = "sales")
public class ContractEntity {

    @Id
    private String id;

    private String name;
    private String description;
    private Boolean deleted;

    @Column(name = "end_contract")
    private LocalDate endContract;

    @OneToOne
    @JoinColumn(name = "customer_id", referencedColumnName = "id")
    private SubjectEntity subject;

    @OneToOne
    @JoinColumn(name = "connected_contract_id", referencedColumnName = "id")
    private ContractEntity connectedContract;

    @OneToOne
    @JoinColumn(name = "owner_id", referencedColumnName = "id")
    private UserEntity userEntity;

    @Column(name = "subject")
    private String contractSubject;

    @Column(name = "valid_from")
    private LocalDate validityStart;

    @Column(name = "effect_from")
    private LocalDate effectStart;

    private Boolean addition;

    @OneToOne
    @JoinColumn(name = "contract_party", referencedColumnName = "id")
    private SubjectEntity contractParty;

    @OneToOne
    @JoinColumn(name = "type", referencedColumnName = "id")
    private ContractTypeEntity type;

    @Column(name = "evidence_no")
    private String evidenceNo;

    private Integer sequence;

    private Integer subsequence;

    @OneToOne
    @JoinColumn(name = "contract_state_id", referencedColumnName = "id")
    private EnumerationEntity contractState;

    @Column(name = "price_no_vat")
    private BigDecimal priceNoVat;

    @Column(name = "maturity_invoice")
    private Integer maturityInvoice;

    @OneToOne
    @JoinColumn(name = "opportunity_id", referencedColumnName = "id")
    private OpportunityEntity opportunityEntity;

    @Enumerated(EnumType.STRING)
    private Signed signed;

    @Column(name = "days_to_notification")
    private Integer sendNotificationDaysBefore;

    @Column(name = "last_update")
    private LocalDateTime lastUpdate;

    @Column(name = "currency")
    private String appCurrency;

    @Column(name = "contract_internal_type")
    private String contractInternalType;

    @OneToMany(fetch = FetchType.EAGER)
    @JoinColumn(name = "object_id")
    private Set<AreaTechnologySignEntity> areaTechnologySignEntitySet;
}
