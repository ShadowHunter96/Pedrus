package cz.bbn.cerberus.attendance.dto;

import lombok.Getter;
import lombok.Setter;


import java.time.LocalDate;


@Getter
@Setter
public class AttendanceDto {
    private String employeeName;
    private String acronym;
    private LocalDate dismissDate;
    private LocalDate startDate;
    private Double holiday;
    private Integer homeOffice;
    private Integer businessTrip;
    private Integer paidLeave;
    private Integer unpaidLeave;
    private Integer month;
    private Integer year;
    private Double workDays;
    private Double work;
    private Double hours;
    private Integer foodVouchers;
    private Integer ill;

    public AttendanceDto() {
        this.holiday = 0D;
        this.ill = 0;
        this.homeOffice = 0;
        this.businessTrip = 0;
        this.paidLeave = 0;
        this.unpaidLeave = 0;
        this.work = 0D;
        this.foodVouchers = 0;
        this.workDays = 0D;
        this.hours = 0D;
    }

    public void plusHoliday(Double num){
        this.holiday = this.holiday + num;
    }

    public void plusIll(Integer num){
        this.ill = this.ill + num;
    }

    public void plusUnpaidLeave(Integer num){
        this.unpaidLeave = this.unpaidLeave + num;
    }

    public void plusPaidLeave(Integer num){
        this.paidLeave = this.paidLeave + num;
    }

    public void plusHomeOffice(Integer num){
        this.homeOffice = this.homeOffice + num;
    }

    public void plusHours(Double num){
        this.hours = this.hours + num;
    }

    public void plusBusinessTrip(Integer num){
        this.businessTrip = this.businessTrip + num;
    }

    public void plusFoodVoucher(Integer num){
        this.foodVouchers = this.foodVouchers + num;
    }

    public void plusWork(Double num){
        this.work = this.work + num;
    }

    public void plusWorkDays(Integer num){
        this.workDays = this.workDays + num;
    }
}
