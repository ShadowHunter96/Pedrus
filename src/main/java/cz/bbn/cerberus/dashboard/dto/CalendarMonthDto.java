package cz.bbn.cerberus.dashboard.dto;

import cz.bbn.cerberus.azure.dto.OutlookEventDto;
import cz.bbn.cerberus.holiday.persistance.HolidayEntity;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.task.dto.TaskDto;
import cz.bbn.cerberus.workreport.dto.DayWorkReportDto;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
public class CalendarMonthDto {

    private List<ProjectDto> projectDtoList = new ArrayList<>();
    private List<InvoiceDto> invoiceDtoList = new ArrayList<>();
    private List<HolidayEntity> holidayEntityList = new ArrayList<>();
    private List<TaskDto> taskDtoList = new ArrayList<>();
    private List<OutlookEventDto> outlookEventList = new ArrayList<>();
    private List<DayWorkReportDto> dayWorkReportList = new ArrayList<>();
}
