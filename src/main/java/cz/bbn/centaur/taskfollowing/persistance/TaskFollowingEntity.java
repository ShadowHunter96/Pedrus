package cz.bbn.cerberus.taskfollowing.persistance;


import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "task_following", schema = "other")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class TaskFollowingEntity {

    @EmbeddedId
    private TaskFollowingId id;

}
