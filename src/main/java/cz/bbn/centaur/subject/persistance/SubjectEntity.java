package cz.bbn.cerberus.subject.persistance;

import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.suppliertype.persistance.SupplierTypeEntity;
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
import java.time.LocalDateTime;

@Entity
@Getter
@Setter
@Table(name = "subject", schema = "sales")
public class SubjectEntity {

    @Id
    private String id;

    private String name;
    private String description;
    private String url;
    private Boolean deleted;

    private String court;
    @Column(name = "file_number")
    private String fileNumber;
    private String register;

    private String ico;
    @Column(name = "company_name")
    private String companyName;
    @Column(name = "law_form")
    private String lawForm;
    private String address;
    @Column(name = "enlist_date")
    private String enlistDate;

    private String capital;

    private String companions;

    private String dic;
    private String reliable;
    @Column(name = "unreliable_from")
    private String unreliableFrom;
    @Column(name = "standard_account")
    private String standardAccount;
    @Column(name = "non_standard_account")
    private String nonStandardAccount;

    @OneToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserEntity userEntity;

    @Column(name = "local_subject")
    private Boolean localSubject;

    @Column(name = "customer")
    private Boolean customer;

    @Column(name = "supplier")
    private Boolean supplier;

    @Column(name = "own_company")
    private Boolean ownCompany;

    @OneToOne
    @JoinColumn(name = "supplier_type_id", referencedColumnName = "id")
    private SupplierTypeEntity supplierType;

    @Column(name = "currency")
    @Enumerated(EnumType.STRING)
    private AppCurrency appCurrency;

    @Column(name = "last_ares_update")
    private LocalDateTime lastUpdateFromAres;
}
