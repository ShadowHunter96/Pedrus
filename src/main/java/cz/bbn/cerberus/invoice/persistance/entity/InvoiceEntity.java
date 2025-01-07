package cz.bbn.cerberus.invoice.persistance.entity;

import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.contract.persistence.entity.ContractEntity;
import cz.bbn.cerberus.dph.persistance.entity.DphEntity;
import cz.bbn.cerberus.user.persistance.UserEntity;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.time.LocalDate;

@Entity
@Table(name = "invoice", schema = "sales")
@Getter
@Setter
public class InvoiceEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "invoice_date")
    private LocalDate invoicingDate;

    @Column(name = "issue_date")
    private LocalDate issueDate;

    @OneToOne
    @JoinColumn(name = "contract_id", referencedColumnName = "id")
    private ContractEntity contractEntity;

    private String description;

    @Column(name = "price_no_vat")
    private Double priceNoVat;

    @OneToOne
    @JoinColumn(name = "dph_id", referencedColumnName = "id")
    private DphEntity dph;

    @Column(name = "price_total")
    private Double priceTotal;

    @Column(name = "tax_date")
    private LocalDate taxDate;

    @Column(name = "days_to_pay")
    private Integer daysToPay;

    @Column(name = "payment_date")
    private LocalDate paymentDate;

    @Column(name = "invoice_no")
    private String invoiceNo;

    private String addressee;

    @Column(name = "transfer_protocol")
    private Boolean transferProtocol;

    @Column(name = "document_id")
    private String documentName;

    private Boolean deleted;

    @Column(name = "created_in_pohoda")
    private Boolean createdInPohoda;

    @Enumerated(EnumType.STRING)
    @Column(name = "currency")
    private AppCurrency appCurrency;

    @OneToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserEntity userEntity;

    private String state;

    @Column(name = "reminder_work_days")
    private Integer reminderWorkDays;

    @Column(name = "string_id")
    private String stringId;
}
