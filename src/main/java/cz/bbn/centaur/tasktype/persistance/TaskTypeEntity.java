package cz.bbn.cerberus.tasktype.persistance;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import java.util.Set;

@Entity
@Table(name = "task_type", schema = "other")
@Getter
@Setter
@NoArgsConstructor
public class TaskTypeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String name;

    private String description;

    private Boolean archived;

    @Column(name = "object_type")
    private String objectType;

    @OneToMany(fetch = FetchType.EAGER)
    @JoinColumn(name = "task_type_id", updatable = false, insertable = false)
    private Set<TaskTypeRoleEntity> taskTypeRoleEntitySet;

}
