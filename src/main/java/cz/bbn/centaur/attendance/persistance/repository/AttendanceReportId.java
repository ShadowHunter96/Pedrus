package cz.bbn.cerberus.attendance.persistance.repository;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import java.io.Serializable;
import java.time.LocalDate;

@Embeddable
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class AttendanceReportId implements Serializable {

    @Column(name = "employee_id")
    private String employeeId;

    @Column(name = "date")
    private LocalDate date;
}
