package cz.bbn.cerberus.tasktemplate.persistance.entity;

import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.role.persistance.entity.RoleEntity;
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
import java.util.Set;

@Entity
@Table(name = "task_template", schema = "other")
@Getter
@Setter
public class TaskTemplateEntity {

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
    @JoinColumn(name = "allowed_role", referencedColumnName = "id")
    private RoleEntity allowedRole;

    @OneToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserEntity userEntity;

    @Column(name = "send_task")
    private String sendTask;

    @OneToMany(fetch = FetchType.EAGER)
    @JoinColumn(name = "task_template_id", updatable = false, insertable = false)
    private Set<TaskTemplateUserEntity> taskTemplateUserEntitySet;

    @OneToMany(fetch = FetchType.EAGER)
    @JoinColumn(name = "task_template_id", updatable = false, insertable = false)
    private Set<TaskTemplateRoleEntity> taskTemplateRoleEntitySet;

    private Boolean deleted;

    @Column(name = "send_to_outlook")
    private Boolean sendToOutlook;

    @OneToOne
    @JoinColumn(name = "task_type_id", referencedColumnName = "id")
    private TaskTypeEntity taskType;

}
