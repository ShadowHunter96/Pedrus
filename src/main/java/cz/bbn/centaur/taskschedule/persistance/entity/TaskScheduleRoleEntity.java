package cz.bbn.cerberus.taskschedule.persistance.entity;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "task_schedule_role", schema = "other")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class TaskScheduleRoleEntity {

    @EmbeddedId
    private TaskScheduleRoleId id;
}
