package cz.bbn.cerberus.workreport.dto;

import lombok.Getter;
import lombok.Setter;

import java.time.LocalDate;
import java.util.List;

@Getter
@Setter
public class DayWorkReportDto {

    private Double hoursTotal;
    private LocalDate date;
    private List<WorkReportDto> workReportDtoList;

}
