package cz.bbn.cerberus.task.persitance.entity;

import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.tasktype.persistance.TaskTypeEntity;
import cz.bbn.cerberus.user.persistance.UserEntity;
import lombok.Getter;
import lombok.NoArgsConstructor;
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
import java.time.LocalDateTime;
import java.util.Objects;
import java.util.Set;

@Entity
@Table(name = "task", schema = "other")
@Getter
@Setter
@NoArgsConstructor
public class TaskEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String name;
    private String description;

    @Column(name = "send_time")
    private LocalDateTime date;

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
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserEntity userEntity;

    @OneToMany(fetch = FetchType.EAGER)
    @JoinColumn(name = "task_id", updatable = false, insertable = false)
    private Set<TaskUserEntity> taskUserEntitySet;

    @OneToMany(fetch = FetchType.EAGER)
    @JoinColumn(name = "task_id", updatable = false, insertable = false)
    private Set<TaskRoleEntity> taskRoleEntitySet;

    @Column(name = "send_task")
    private String sendTask;

    @Column(name = "creation_date")
    private LocalDateTime creationDate;

    @OneToOne
    @JoinColumn(name = "task_type_id", referencedColumnName = "id")
    private TaskTypeEntity taskType;

    @OneToOne
    @JoinColumn(name = "assignee_id", referencedColumnName = "id")
    private UserEntity assignee;

    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        TaskEntity that = (TaskEntity) o;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}
