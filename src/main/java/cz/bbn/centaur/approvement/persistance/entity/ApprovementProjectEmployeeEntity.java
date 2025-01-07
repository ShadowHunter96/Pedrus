package cz.bbn.cerberus.approvement.persistance.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "approvement_project_employee", schema = "intranet")
@Getter
@Setter
@NoArgsConstructor
public class ApprovementProjectEmployeeEntity {

    @EmbeddedId
    private ApprovementProjectEmployeeId id;

    public ApprovementProjectEmployeeEntity(ApprovementProjectEmployeeId id) {
        this.id = id;
    }

}
