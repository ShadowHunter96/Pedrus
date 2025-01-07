package cz.bbn.cerberus.task.persitance.entity;

import cz.bbn.cerberus.user.persistance.UserEntity;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.time.LocalDateTime;

@Entity
@Table(name = "task_check", schema = "other")
@Getter
@Setter
@NoArgsConstructor
public class TaskCheckEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "entity_type")
    private String entityType;

    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "checkbox_name")
    private String checkboxName;

    private Boolean value;

    @OneToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserEntity user;

    @Column(name = "complete_date")
    private LocalDateTime completeDate;

    @Column(name = "days_to_finish")
    private Integer daysToFinish;
}
