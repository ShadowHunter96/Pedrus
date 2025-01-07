package cz.bbn.cerberus.workreport.persistance.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;

@Getter
@Setter
@Entity
@Table(name = "work_report_closed", schema = "intranet")
@ToString
@NoArgsConstructor
public class WorkReportClosedEntity {

    @EmbeddedId
    private WorkReportClosedId id;

    public WorkReportClosedEntity(WorkReportClosedId id) {
        this.id = id;
    }
}
