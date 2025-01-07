package cz.bbn.cerberus.project.persistance.entity;

import cz.bbn.cerberus.contract.persistence.entity.ContractEntity;
import cz.bbn.cerberus.project.ProjectState;
import cz.bbn.cerberus.subject.persistance.SubjectEntity;
import cz.bbn.cerberus.user.persistance.UserEntity;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.time.LocalDate;

@Entity
@Table(name = "project", schema = "sales")
@Getter
@Setter
public class ProjectEntity {

    @Id
    private String id;

    private String name;
    private String description;

    @OneToOne
    @JoinColumn(name = "subject_id", referencedColumnName = "id")
    private SubjectEntity subject;

    @OneToOne
    @JoinColumn(name = "contract_id", referencedColumnName = "id")
    private ContractEntity contract;

    private Boolean deleted;

    @Column(name = "start_time")
    private LocalDate startTime;
    @Column(name = "end_time")
    private LocalDate endTime;

    private String color;

    @OneToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserEntity userEntity;

    @Enumerated(EnumType.STRING)
    @Column(name = "project_state")
    private ProjectState projectState;

}
