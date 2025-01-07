package cz.bbn.cerberus.task.persitance.entity;

import cz.bbn.cerberus.user.persistance.UserEntity;
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
public class TaskUserId implements Serializable {

    @OneToOne
    @JoinColumn(name = "task_id", referencedColumnName = "id")
    private TaskEntity taskEntity;

    @OneToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserEntity userEntity;
}
