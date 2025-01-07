package cz.bbn.cerberus.attendance.dto;

import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
public class AttendanceReportSummaryDto {

    private List<AttendanceReportDto> attendanceReportDtoList;
    private Integer year;
    private Integer month;
    private String employeeId;
    private String employeeName;
    private Long userId;
    private String acronym;
    private Double reportWork;
    private Integer reportDays;
    private Integer reportFoodVouchers;
    private Integer workDaysFoodVouchers;
    private AttendanceDto attendanceDto;

    public AttendanceReportSummaryDto() {
        this.attendanceReportDtoList = new ArrayList<>();
        this.reportDays = 0;
        this.reportWork = 0D;
        this.reportFoodVouchers = 0;
        this.workDaysFoodVouchers = 0;
        this.attendanceDto = new AttendanceDto();
    }

    public void plusReportFoodVouchers(Integer num){
        this.reportFoodVouchers = this.reportFoodVouchers + num;
    }

    public void plusWorkDaysFoodVouchers(Integer num){
        this.workDaysFoodVouchers = this.workDaysFoodVouchers + num;
    }

    public void plusReportWork(Double num){
        this.reportWork = this.reportWork + num;
    }
}
