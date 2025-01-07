package cz.bbn.cerberus.taskschedule.persistance.entity;

import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.tasktype.persistance.TaskTypeEntity;
import cz.bbn.cerberus.user.persistance.UserEntity;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.time.LocalDate;
import java.util.Set;

@Entity
@Table(name = "task_schedule", schema = "other")
@Getter
@Setter
public class TaskScheduleEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String name;
    private String description;

    @Enumerated(EnumType.STRING)
    @Column(name = "object_type")
    private ObjectType objectType;

    @Column(name = "object_id")
    private String objectId;

    @Column(name = "subject_id")
    private String subjectId;

    private String state;

    @Column(name = "days_to_notify")
    private Integer daysToNotify;

    @Column(name = "notify_frequency")
    private String notifyFrequency;

    private String color;

    @OneToOne
    @JoinColumn(name = "assignee_id", referencedColumnName = "id")
    private UserEntity assignee;

    @OneToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserEntity userEntity;

    @Column(name = "send_task")
    private String sendTask;

    @OneToMany(fetch = FetchType.EAGER)
    @JoinColumn(name = "task_schedule_id", updatable = false, insertable = false)
    private Set<TaskScheduleUserEntity> taskScheduleUserEntitySet;

    @OneToMany(fetch = FetchType.EAGER)
    @JoinColumn(name = "task_schedule_id", updatable = false, insertable = false)
    private Set<TaskScheduleRoleEntity> taskScheduleRoleEntitySet;

    private Boolean deleted;

    @Column(name = "send_to_outlook")
    private Boolean sendToOutlook;

    private String frequency;

    @Column(name = "creation_day")
    private Integer creationDay;

    @Column(name = "creation_date")
    private LocalDate creationDate;

    @OneToOne
    @JoinColumn(name = "task_type_id", referencedColumnName = "id")
    private TaskTypeEntity taskType;
}
