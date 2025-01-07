package cz.bbn.cerberus.dashboard.ui.component;

import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.azure.AzureGraphService;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.dashboard.dto.CalendarMonthDto;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.holiday.HolidayService;
import cz.bbn.cerberus.holiday.persistance.HolidayEntity;
import cz.bbn.cerberus.invoice.InvoicingService;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.project.ProjectService;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.task.TaskService;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.workreport.WorkReportComponentOperation;
import cz.bbn.cerberus.workreport.dto.DayWorkReportDto;
import cz.bbn.cerberus.workreport.dto.ProjectPhaseActivityDto;
import cz.bbn.cerberus.workreport.dto.WorkReportClosedDto;
import cz.bbn.cerberus.workreport.dto.WorkReportDto;
import cz.bbn.cerberus.workreport.ui.component.WorkReportDialog;
import cz.bbn.cerberus.workreport.ui.component.WorkReportSaveListener;
import lombok.extern.slf4j.Slf4j;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.format.TextStyle;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.stream.Collectors;

@Slf4j
public class DashboardTab extends TabSimpleComponent implements DashboardListener, WorkReportSaveListener {

    private final ProjectService projectService;
    private final InvoicingService invoicingService;
    private final HolidayService holidayService;
    private final ListService listService;
    private final TaskService taskService;
    private final AzureGraphService azureGraphService;
    private final UserService userService;
    private final WorkReportComponentOperation workReportComponentOperation;

    private DashboardCalenderComponent calendarComponent;
    private EmployeeDto employeeDto;
    private Set<String> closedReports;
    private List<ProjectDto> projectList;
    private List<HolidayEntity> holidayList;

    public DashboardTab(ProjectService projectService, InvoicingService invoicingService,
                        HolidayService holidayService, ListService listService, TaskService taskService,
                        AzureGraphService azureGraphService, UserService userService,
                        WorkReportComponentOperation workReportComponentOperation) {
        this.projectService = projectService;
        this.invoicingService = invoicingService;
        this.holidayService = holidayService;
        this.listService = listService;
        this.taskService = taskService;
        this.azureGraphService = azureGraphService;
        this.userService = userService;
        this.workReportComponentOperation = workReportComponentOperation;
        initView();
    }

    private void initView() {
        setSizeFull();

        this.projectList = listService.getAllowedProjectDtoList();
        this.holidayList = holidayService.findAll();

        employeeDto = null;
        try {
            UserDto userDto = userService.getUser(SecurityUtils.getCurrentUserId());
            if (userDto.getEmployee() != null) {
                employeeDto = userDto.getEmployee();
            }
        } catch (SystemException e) {
            log.error(e.getMessage(), e);
        }

        closedReports = workReportComponentOperation.getClosedReports(employeeDto).stream()
                .map(WorkReportClosedDto::getYearMonth).collect(Collectors.toSet());

        calendarComponent = new DashboardCalenderComponent(
                getMonthData(LocalDate.now()), listService.getContractMap(), employeeDto, this);

        this.add(calendarComponent);
    }

    private Set<LocalDate> getMonthHolidaySet(LocalDate date) {
        Set<String> yearMonthSet = new HashSet<>();
        yearMonthSet.add(date.getYear() + date.getMonth().getDisplayName(TextStyle.FULL, Locale.ENGLISH));
        yearMonthSet.add(date.plusMonths(1).getYear() +
                date.plusMonths(1).getMonth().getDisplayName(TextStyle.FULL, Locale.ENGLISH));
        yearMonthSet.add(date.plusMonths(2).getYear() +
                date.plusMonths(2).getMonth().getDisplayName(TextStyle.FULL, Locale.ENGLISH));
        Set<LocalDate> monthHolidaySet = new HashSet<>();
        for (HolidayEntity holiday : holidayList) {
            LocalDate holidayDate = holiday.getDate();
            String holidayString = holidayDate.getYear() +
                    holidayDate.getMonth().getDisplayName(TextStyle.FULL, Locale.ENGLISH);
            if (yearMonthSet.contains(holidayString)) {
                monthHolidaySet.add(holidayDate);
            }
        }
        return monthHolidaySet;
    }

    private Set<DayWorkReportDto> generateEmptyReports(Set<LocalDate> dateSet, LocalDate monthBeginning) {
        Set<DayWorkReportDto> reportSet = new HashSet<>();

        Set<Month> monthSet = new HashSet<>();
        monthSet.add(monthBeginning.getMonth());
        monthSet.add(monthBeginning.plusMonths(1).getMonth());
        monthSet.add(monthBeginning.plusMonths(2).getMonth());

        int i = 0;

        while (true) {
            LocalDate currentDate = monthBeginning.plusDays(i);
            i++;
            if (!monthSet.contains(currentDate.getMonth())) {
                break;
            }
            if (!dateSet.contains(currentDate) && currentDate.getDayOfWeek().getValue() != 6
                    && currentDate.getDayOfWeek().getValue() != 7) {
                DayWorkReportDto dayWorkReportDto = new DayWorkReportDto();
                dayWorkReportDto.setDate(currentDate);
                dayWorkReportDto.setHoursTotal(0D);
                dayWorkReportDto.setWorkReportDtoList(new ArrayList<>());
                reportSet.add(dayWorkReportDto);
            }
        }
        return reportSet;
    }

    @Override
    public void openWorkReport(LocalDate day) {
        WorkReportDto workReportDto = new WorkReportDto();
        workReportDto.setDate(day);
        ProjectPhaseActivityDto projectPhaseActivityDto =
                workReportComponentOperation.getProjectPhaseActivityDto(employeeDto);
        List<Double> durationList = workReportComponentOperation.getDurationValues();
        boolean enableActions = true;
        String monthYear = day.getMonth().getDisplayName(TextStyle.FULL, Locale.ENGLISH) + day.getYear();
        if (closedReports.contains(monthYear)) {
            enableActions = false;
        }
        WorkReportDialog workReportDialog =
                new WorkReportDialog(workReportDto, projectPhaseActivityDto, durationList, this, enableActions, false);
        workReportDialog.open();
    }

    @Override
    public CalendarMonthDto getMonthData(LocalDate date) {
        LocalDateTime start = AppUtils.getMonthStartWithTime(date.minusMonths(1));
        LocalDateTime end = AppUtils.getMonthEndWithTime(date.plusMonths(1));

        CalendarMonthDto monthDto = new CalendarMonthDto();

        List<DayWorkReportDto> dayWorkReportList =
                workReportComponentOperation.getDayWorkReport(start.toLocalDate(), end.toLocalDate(), employeeDto);

        Set<LocalDate> dateSet =
                dayWorkReportList.stream().map(DayWorkReportDto::getDate).collect(Collectors.toSet());
        dateSet.addAll(getMonthHolidaySet(date));

        dayWorkReportList.addAll(generateEmptyReports(dateSet, start.toLocalDate()));

        monthDto.setTaskDtoList(taskService.findTaskEntityByUser(start, end));
        monthDto.setInvoiceDtoList(
                invoicingService.findInvoiceDtoByAllowedContractList(start.toLocalDate(), end.toLocalDate()));
        monthDto.setProjectDtoList(projectList);
        monthDto.setHolidayEntityList(holidayList);
        monthDto.setOutlookEventList(azureGraphService.getOutlookEventDtoList(start, end));
        monthDto.setDayWorkReportList(dayWorkReportList);

        return monthDto;
    }

    @Override
    public void save(Binder<WorkReportDto> binder, AppDialog dialog, WorkReportDto originalDto, boolean showDialog, List<HolidayEntity> holidayEntityList) {
        if (binder.validate().isOk()) {
            WorkReportDto workReportDto = binder.getBean();
            workReportComponentOperation.getSaveAction(employeeDto).saveItem(workReportDto, originalDto);
            calendarComponent.setLists(getMonthData(calendarComponent.getCurrentDate()));
            calendarComponent.refreshCalendar();
            if (dialog != null) {
                dialog.close();
            }
        }
    }
}
