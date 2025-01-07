package cz.bbn.cerberus.employee.persistance.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Entity
@Table(name = "employee_by_object", schema = "backoffice")
@Getter
@Setter
@NoArgsConstructor
public class EmployeeByObjectEntity {

    @EmbeddedId
    private EmployeeByObjectId id;

    @OneToOne
    @JoinColumn(name = "employee_id", referencedColumnName = "id", insertable = false, updatable = false)
    private EmployeeEntity employeeEntity;

    public EmployeeByObjectEntity(EmployeeByObjectId id) {
        this.id = id;
    }
}
