package cz.bbn.cerberus.suppliertype.persistance;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "supplier_type", schema = "enums")
@Getter
@Setter
@NoArgsConstructor
public class SupplierTypeEntity {

    @Id
    private String id;

    private String name;
    private Boolean allowed;

    public SupplierTypeEntity(String id) {
        this.id = id;
    }
}
