package cz.bbn.cerberus.approvement.persistance.entity;

import cz.bbn.cerberus.employee.persistance.entity.EmployeeEntity;
import cz.bbn.cerberus.project.persistance.entity.ProjectEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import java.io.Serializable;

@Embeddable
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class ApprovementProjectEmployeeId implements Serializable {

    @Column(name = "approvement_id")
    private Long approvementId;

    @OneToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "project_id", referencedColumnName = "id")
    private ProjectEntity projectEntity;

    @OneToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "employee_id", referencedColumnName = "id")
    private EmployeeEntity employeeEntity;

}
