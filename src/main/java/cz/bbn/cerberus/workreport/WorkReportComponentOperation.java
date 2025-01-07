package cz.bbn.cerberus.workreport;

import cz.bbn.cerberus.activity.ActivityByObjectService;
import cz.bbn.cerberus.activity.dto.ActivityByObjectDto;
import cz.bbn.cerberus.approvement.enums.ApprovementType;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.employee.EmployeeService;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.phase.PhaseService;
import cz.bbn.cerberus.phase.dto.PhaseDto;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.workreport.dto.DayWorkReportDto;
import cz.bbn.cerberus.workreport.dto.ProjectPhaseActivityDto;
import cz.bbn.cerberus.workreport.dto.WorkReportClosedDto;
import cz.bbn.cerberus.workreport.dto.WorkReportDto;
import cz.bbn.cerberus.workreport.dto.WorkReportPickDto;
import cz.bbn.cerberus.workreport.dto.YearMonthDto;
import cz.bbn.cerberus.workreport.persistance.entity.WorkReportEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@Component
@Slf4j
public class WorkReportComponentOperation {

    private final EmployeeService employeeService;
    private final PhaseService phaseService;
    private final UserService userService;
    private final ActivityByObjectService activityByObjectService;
    private final WorkReportService workReportService;
    private final ListService listService;
    private final AppEnv appEnv;

    public WorkReportComponentOperation(EmployeeService employeeService, PhaseService phaseService,
                                        UserService userService, ActivityByObjectService activityByObjectService,
                                        WorkReportService workReportService, ListService listService, AppEnv appEnv) {
        this.employeeService = employeeService;
        this.phaseService = phaseService;
        this.userService = userService;
        this.activityByObjectService = activityByObjectService;
        this.workReportService = workReportService;
        this.listService = listService;
        this.appEnv = appEnv;
    }

    public ProjectPhaseActivityDto getProjectPhaseActivityDto(EmployeeDto employeeDto) {
        if (employeeDto != null) {
            return getProjectPhaseActivity(employeeDto.getId());
        }
        return new ProjectPhaseActivityDto();
    }

    private ProjectPhaseActivityDto getProjectPhaseActivity(String employeeId) {
        List<ObjectType> objectTypeSet = new ArrayList<>();
        objectTypeSet.add(ObjectType.PROJECT);
        objectTypeSet.add(ObjectType.OPPORTUNITY);
        List<String> projectIdList = employeeService.getObjectIdByEmployeeAndType(employeeId, objectTypeSet);

        List<WorkReportPickDto> pickList = new ArrayList<>();
        Map<String, List<PhaseDto>> projectIdPhaseMap = new HashMap<>();
        Map<String, List<EnumerationDto>> projectIdActivityMap = new HashMap<>();
        for (WorkReportPickDto dto : getPickRawList()) {
            if (projectIdList.contains(dto.getId())) {
                pickList.add(dto);
                projectIdPhaseMap.put(dto.getId(), new ArrayList<>());
                projectIdActivityMap.put(dto.getId(), new ArrayList<>());
            }
        }

        List<PhaseDto> phaseList = phaseService.getPhaseList();
        for (PhaseDto phaseDto : phaseList) {
            if (projectIdPhaseMap.containsKey(phaseDto.getProjectId())) {
                List<PhaseDto> tempList = projectIdPhaseMap.get(phaseDto.getProjectId());
                tempList.add(phaseDto);
                projectIdPhaseMap.put(phaseDto.getProjectId(), tempList);
            }
        }
        List<ActivityByObjectDto> activityList = activityByObjectService.findAllAllowedList();
        for (ActivityByObjectDto activityDto : activityList) {
            if (projectIdActivityMap.containsKey(activityDto.getId().getObjectId())) {
                List<EnumerationDto> tempList = projectIdActivityMap.get(activityDto.getId().getObjectId());
                tempList.add(activityDto.getEnumerationDto());
                projectIdActivityMap.put(activityDto.getId().getObjectId(), tempList);
            }
        }

        ProjectPhaseActivityDto projectPhaseActivityDto = new ProjectPhaseActivityDto();
        projectPhaseActivityDto.setPickItemList(pickList);
        projectPhaseActivityDto.setProjectPhaseMap(projectIdPhaseMap);
        projectPhaseActivityDto.setProjectActivityMap(projectIdActivityMap);

        return projectPhaseActivityDto;
    }

    public SaveAction<WorkReportDto> getSaveAction(EmployeeDto employeeDto) {
        return (dto, originalDto) -> {
            if (workReportValidate(dto, employeeDto)) {
                dto.setEmployeeId(employeeDto.getId());
                workReportService.save(dto);
                SuccessNotification.showSavingSuccess(appEnv);
            }
        };
    }

    private boolean workReportValidate(WorkReportDto dto, EmployeeDto employeeDto){
        boolean validate = true;
        if (employeeDto == null) {
            validate = false;
            ErrorNotification.show(Transl.get(ErrorCode.EMPLOYEE_NOT_FOUND.getError()), appEnv);
        }else{
            WorkReportEntity workReportEntity = workReportService.getWorkReportWithApprovement(dto, employeeDto);
            if(workReportEntity != null) {
                ApprovementType type = workReportEntity.getApprovementEntity().getApprovementType();
                if (workReportEntity.getApprovementEntity().getHalfDay() && type.equals(ApprovementType.HOLIDAY) && dto.getDuration() > 4D) {
                    validate = false;
                    ErrorNotification.show(Transl.get("You have entered half a day off for this period. Therefore, you cannot report more than 4 hours of work"), appEnv);
                } else if (!type.equals(ApprovementType.HOME_OFFICE) && !type.equals(ApprovementType.BUSSINES_TRIP)
                        && dto.getDuration() > 0D && workReportEntity.getApprovementEntity().getHalfDay().equals(Boolean.FALSE)) {
                    validate = false;
                    ErrorNotification.show(Transl.get("A request is entered for this day - type ").concat(Transl.get(type.name())), appEnv);
                }
            }
        }
        return validate;
    }

    public List<DayWorkReportDto> getDayWorkReport(LocalDate start, LocalDate end, EmployeeDto employeeDto) {
        String empId;
        if (employeeDto != null) {
            empId = employeeDto.getId();
        } else {
            return new ArrayList<>();
        }

        List<WorkReportDto> workReportList = fillWorkReport(
                workReportService.getPeriodWorkReportDtoList(start, end, empId));
        List<DayWorkReportDto> dayWorkReportDtoList = getProcessedDayWorkReportList(workReportList);
        dayWorkReportDtoList.sort(Comparator.comparing(DayWorkReportDto::getDate).reversed());
        return dayWorkReportDtoList;
    }

    private List<DayWorkReportDto> getProcessedDayWorkReportList(List<WorkReportDto> workReportList) {
        Map<LocalDate, List<WorkReportDto>> workReportMap = new HashMap<>();
        for (WorkReportDto workReportDto : workReportList) {
            List<WorkReportDto> tempList;
            if (workReportMap.containsKey(workReportDto.getDate())) {
                tempList = workReportMap.get(workReportDto.getDate());
            } else {
                tempList = new ArrayList<>();
            }
            tempList.add(workReportDto);
            workReportMap.put(workReportDto.getDate(), tempList);
        }
        List<DayWorkReportDto> dayWorkReportDtoList = new ArrayList<>();
        for (LocalDate localeDate : workReportMap.keySet()) {
            DayWorkReportDto dayWorkReportDto = new DayWorkReportDto();
            List<WorkReportDto> workReportDtoList = workReportMap.get(localeDate);
            workReportDtoList.sort(Comparator.comparing(WorkReportDto::getId));
            double totalHours = 0.0;
            for (WorkReportDto workReportDto : workReportDtoList) {
                totalHours = totalHours + Optional.ofNullable(workReportDto.getDuration()).orElse(0d);
            }
            dayWorkReportDto.setDate(localeDate);
            dayWorkReportDto.setHoursTotal(totalHours);
            dayWorkReportDto.setWorkReportDtoList(workReportDtoList);
            dayWorkReportDtoList.add(dayWorkReportDto);
        }
        return dayWorkReportDtoList;
    }

    public void deleteWorkReport(Long id) {
        workReportService.deleteWorkReport(id);
    }

    public ByteArrayOutputStream getAssetPdf(List<DayWorkReportDto> dayWorkReportDtoList, String userName,
                                             String month, String year) throws IOException {
        return workReportService.getAssetPdf(dayWorkReportDtoList, userName, month, year);
    }

    public String getCurrentUserName(EmployeeDto employeeDto) {
        if (employeeDto != null) {
            return employeeDto.getLastName() + " " + employeeDto.getFirstName();
        }
        return "";
    }

    public EmployeeDto getCurrentEmployee() {
        try {
            UserDto currentUser = userService.getUser(SecurityUtils.getCurrentUserId());
            if (currentUser.getEmployee() != null) {
                return currentUser.getEmployee();
            } else {
                ErrorNotification.show(Transl.get(ErrorCode.EMPLOYEE_NOT_FOUND.getError()), appEnv);
                return null;
            }
        } catch (SystemException e) {
            log.error(TextValues.SYSTEM_EXCEPTION, e);
            ErrorNotification.show(Transl.get(ErrorCode.EMPLOYEE_NOT_FOUND.getError()), appEnv);
            return null;
        }
    }

    public List<WorkReportClosedDto> getClosedReports(EmployeeDto employeeDto) {
        if (employeeDto != null) {
            return workReportService.getWorkReportClosedList(employeeDto.getId());
        }
        return new ArrayList<>();
    }

    public void closeWorkReport(YearMonthDto yearMonthDto, EmployeeDto employeeDto) {
        if (employeeDto != null) {
            workReportService.closeWorkReport(yearMonthDto, employeeDto);
        } else {
            ErrorNotification.show(Transl.get(ErrorCode.EMPLOYEE_NOT_FOUND.getError()), appEnv);
        }
    }

    public List<Double> getDurationValues() {
        List<Double> durationList = new ArrayList<>();
        durationList.add(0.5);
        double beginning = 0.5;
        while (beginning < 12.0) {
            durationList.add(beginning + 0.5);
            beginning = beginning + 0.5;
        }
        return durationList;
    }

    private List<WorkReportDto> fillWorkReport(List<WorkReportDto> workReportDtoList) {
        List<WorkReportPickDto> workReportRawList = getPickRawList();
        Map<String, WorkReportPickDto> workReportMap = new HashMap<>();
        for (WorkReportPickDto rawDto : workReportRawList) {
            workReportMap.put(rawDto.getId(), rawDto);
        }

        List<WorkReportDto> workReportList = new ArrayList<>();
        for (WorkReportDto reportDto : workReportDtoList) {
            if (workReportMap.containsKey(reportDto.getItemId())) {
                reportDto.setPickDto(workReportMap.get(reportDto.getItemId()));
                workReportList.add(reportDto);
            }else if(reportDto.getApprovementDto() != null){
                reportDto.setPickDto(new WorkReportPickDto());
                workReportList.add(reportDto);
            }
        }
        return workReportList;
    }

    private List<WorkReportPickDto> getPickRawList() {
        List<WorkReportPickDto> pickList = new ArrayList<>();
        for (ProjectDto projectDto : listService.getProjectDtoList()) {
            WorkReportPickDto pickDto = new WorkReportPickDto();
            pickDto.setId(projectDto.getId());
            pickDto.setName(projectDto.getName());
            pickList.add(pickDto);
        }
        for (OpportunityDto opportunityDto : listService.getOpportunityDtoList()) {
            WorkReportPickDto pickDto = new WorkReportPickDto();
            pickDto.setId(opportunityDto.getId());
            pickDto.setName(opportunityDto.getName());
            pickList.add(pickDto);
        }
        return pickList;
    }
}
