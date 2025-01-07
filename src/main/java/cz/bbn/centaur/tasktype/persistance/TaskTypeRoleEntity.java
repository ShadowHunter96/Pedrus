package cz.bbn.cerberus.tasktype.persistance;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "task_type_role", schema = "other")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class TaskTypeRoleEntity {

    @EmbeddedId
    private TaskTypeRoleId id;
}
