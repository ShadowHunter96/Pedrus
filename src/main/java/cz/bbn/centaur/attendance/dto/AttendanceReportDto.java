package cz.bbn.cerberus.attendance.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDate;


@Getter
@Setter
@NoArgsConstructor
public class AttendanceReportDto {
    private String employeeId;
    private String employeeName;
    private LocalDate date;
    private AttendenceEnum attendenceEnum;
    private boolean foodVoucher;
    private boolean reportFoodVoucher;
    private Double work;

    public AttendanceReportDto(String employeeName) {
        this.employeeName = employeeName;
    }

}
