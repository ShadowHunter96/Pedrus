package cz.bbn.cerberus.contracttype.persistence;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Getter
@Setter
@Table(name = "contract_type", schema = "enums")
@NoArgsConstructor
public class ContractTypeEntity {

    @Id
    private String id;

    private String name;
    private String description;
    private Boolean allowed;
    private Boolean sales;
    private Boolean operational;

    @Column(name = "supplier_co")
    private Boolean supplierCo;

    @Column(name = "connection_required")
    private Boolean connectionRequired;

    @Column(name = "employee_co")
    private Boolean employeeCo;

    public ContractTypeEntity(String id) {
        this.id = id;
    }
}
