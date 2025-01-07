package cz.bbn.cerberus.attendance.persistance.entity;

import cz.bbn.cerberus.attendance.persistance.repository.AttendanceReportId;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.Immutable;

import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "v_attendance_report", schema = "intranet")
@Immutable
@Getter
@Setter
@NoArgsConstructor
public class AttendanceReportEntity {

    @EmbeddedId
    private AttendanceReportId id;

    @Column(name = "employee_name")
    private String employeeName;

    private String acronym;

    @Column(name = "user_id")
    private Long userId;

    @Column(name = "work")
    private Double work;

    @Column(name = "holiday")
    private Double holiday;

    @Column(name = "home_office")
    private Integer homeOffice;

    @Column(name = "business_trip")
    private Integer businessTrip;

    @Column(name = "paid_leave")
    private Integer paidLeave;

    @Column(name = "unpaid_leave")
    private Integer unpaidLeave;

    @Column(name = "ill")
    private Integer ill;

    public AttendanceReportEntity(String employeeName) {
        this.employeeName = employeeName;
    }

}
