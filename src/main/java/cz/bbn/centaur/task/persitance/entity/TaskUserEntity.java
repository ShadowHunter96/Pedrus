package cz.bbn.cerberus.task.persitance.entity;


import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "task_user", schema = "other")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class TaskUserEntity {

    @EmbeddedId
    private TaskUserId id;
}
