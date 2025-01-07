package cz.bbn.cerberus.taskschedule.persistance.entity;

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
public class TaskScheduleRoleId implements Serializable {

    @OneToOne
    @JoinColumn(name = "task_schedule_id", referencedColumnName = "id")
    private TaskScheduleEntity taskScheduleEntity;

    @OneToOne
    @JoinColumn(name = "role_id", referencedColumnName = "id")
    private RoleEntity roleEntity;
}
