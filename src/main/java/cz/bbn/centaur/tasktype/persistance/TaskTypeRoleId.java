package cz.bbn.cerberus.tasktype.persistance;

import cz.bbn.cerberus.role.persistance.entity.RoleEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Embeddable;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import java.io.Serializable;

@Embeddable
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class TaskTypeRoleId implements Serializable {

    @OneToOne
    @JoinColumn(name = "task_type_id", referencedColumnName = "id")
    private TaskTypeEntity taskTypeEntity;

    @OneToOne
    @JoinColumn(name = "role_id", referencedColumnName = "id")
    private RoleEntity roleEntity;
}
