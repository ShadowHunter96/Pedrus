package cz.bbn.cerberus.approvement.persistance.entity;

import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationEntity;
import cz.bbn.cerberus.opportunity.persistance.entity.OpportunityEntity;
import cz.bbn.cerberus.project.persistance.entity.ProjectEntity;
import cz.bbn.cerberus.subject.persistance.SubjectEntity;
import lombok.Getter;
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
@Table(name = "approvement_business_trip", schema = "intranet")
@Getter
@Setter
public class ApprovementBusinessTripEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @OneToOne
    @JoinColumn(name = "from_subject_id", referencedColumnName = "id")
    private SubjectEntity approvementFromSubjectEntity;

    @OneToOne
    @JoinColumn(name = "to_subject_id", referencedColumnName = "id")
    private SubjectEntity approvementToSubjectEntity;

    @Column(name = "address_to_another")
    private String approvementToAnother;

    @OneToOne
    @JoinColumn(name = "purpose_id", referencedColumnName = "id")
    private EnumerationEntity purposeEntity;

    @Column(name = "transportation_type")
    private String businessTripTransportationType;

    @Column(name = "interruption_from")
    private LocalDateTime interruptionFrom;

    @Column(name = "interruption_to")
    private LocalDateTime interruptionTo;

    @OneToOne
    @JoinColumn(name = "project_id", referencedColumnName = "id")
    private ProjectEntity projectEntity;

    @OneToOne
    @JoinColumn(name = "opportunity_id", referencedColumnName = "id")
    private OpportunityEntity opportunityEntity;

    @Column(name = "fellow_passengers")
    private String fellowPassengers;
}
