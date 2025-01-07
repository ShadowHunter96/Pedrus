package cz.bbn.cerberus.approvement.persistance.entity;

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
@Table(name = "approvement_day", schema = "intranet")
@Getter
@Setter
public class ApprovementDayEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @OneToOne
    @JoinColumn(name = "approvement_id", referencedColumnName = "id")
    private ApprovementEntity approvementEntity;

    @Column(name = "date")
    private LocalDateTime date;

}
