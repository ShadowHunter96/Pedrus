package cz.bbn.cerberus.opportunity.persistance.entity;

import cz.bbn.cerberus.opportunity.dto.OpportunityState;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Id;
import javax.persistence.Table;
import java.time.LocalDate;

@Entity
@Table(name = "opportunity", schema = "sales")
@Getter
@Setter
public class OpportunitySimpleEntity {

    @Id
    private String id;

    private String name;
    private String description;

    @Column(name = "subject_id")
    private String subjectId;

    @Enumerated(EnumType.STRING)
    @Column(name = "state")
    private OpportunityState state;

    @Column(name = "user_id")
    private Long userId;

    private Integer progress;

    @Column(name = "success_chance")
    private Integer successChance;

    @Column(name = "realisation_date")
    private LocalDate startDate;

    private Boolean deleted;
}
