package cz.bbn.cerberus.employeecontract.persistance;

import cz.bbn.cerberus.contracttype.persistence.ContractTypeEntity;
import cz.bbn.cerberus.employee.persistance.entity.EmployeeEntity;
import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationEntity;
import cz.bbn.cerberus.subject.persistance.SubjectEntity;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Entity
@Table(name = "employee_contract", schema = "backoffice")
@Getter
@Setter
public class EmployeeContractEntity {

    @Id
    private String id;

    @OneToOne
    @JoinColumn(name = "own_company", referencedColumnName = "id")
    private SubjectEntity ownCompany;

    @OneToOne
    @JoinColumn(name = "type", referencedColumnName = "id")
    private ContractTypeEntity type;

    @OneToOne
    @JoinColumn(name = "employee_id", referencedColumnName = "id")
    private EmployeeEntity employee;

    @Column(name = "linked_contract")
    private String linkedContract;

    private String name;

    @Column(name = "valid_from")
    private LocalDate validFrom;

    @Column(name = "valid_to")
    private LocalDate validTo;

    private Integer reminder;

    @Column(name = "contract_number")
    private String contractNumber;

    @OneToOne
    @JoinColumn(name = "state", referencedColumnName = "id")
    private EnumerationEntity state;

    private String description;

    @Column(name = "create_date")
    private LocalDateTime creationDate;

    private Integer sequence;
    private Integer subsequence;

    private Boolean deleted;
    private Boolean archived;
}
