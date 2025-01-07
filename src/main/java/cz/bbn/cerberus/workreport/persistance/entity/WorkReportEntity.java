package cz.bbn.cerberus.workreport.persistance.entity;

import cz.bbn.cerberus.approvement.persistance.entity.ApprovementEntity;
import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationEntity;
import cz.bbn.cerberus.phase.repository.PhaseEntity;
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
import java.time.LocalDate;

@Entity
@Getter
@Setter
@Table(name = "work_report", schema = "intranet")
public class WorkReportEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private LocalDate date;

    @Column(name = "object_id")
    private String itemId;

    @OneToOne
    @JoinColumn(name = "phase_id", referencedColumnName = "id")
    private PhaseEntity phase;

    @OneToOne
    @JoinColumn(name = "activity_id", referencedColumnName = "id")
    private EnumerationEntity activity;

    @Column(name = "employee_id")
    private String employeeId;

    private Double duration;

    private String description;

    @OneToOne
    @JoinColumn(name = "approvement_id", referencedColumnName = "id")
    private ApprovementEntity approvementEntity;
}
