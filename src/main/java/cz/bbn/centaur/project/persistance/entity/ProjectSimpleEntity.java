package cz.bbn.cerberus.project.persistance.entity;

import cz.bbn.cerberus.project.ProjectState;
import cz.bbn.cerberus.user.persistance.UserEntity;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Entity
@Table(name = "project", schema = "sales")
@Getter
@Setter
@NoArgsConstructor
public class ProjectSimpleEntity {

    @Id
    private String id;

    private String name;

    @Column(name = "subject_id")
    private String subjectId;

    private Boolean deleted;

    @Column(name = "contract_id")
    private String contract;

    @OneToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserEntity userEntity;

    @Enumerated(EnumType.STRING)
    @Column(name = "project_state")
    private ProjectState projectState;

    public ProjectSimpleEntity(String id, String name) {
        this.id = id;
        this.name = name;
    }
}
