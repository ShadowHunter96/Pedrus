package cz.bbn.cerberus.approvement;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.approvement.dto.ApprovementBusinessTripDto;
import cz.bbn.cerberus.approvement.dto.ApprovementDto;
import cz.bbn.cerberus.approvement.dto.ApprovementFilterDto;
import cz.bbn.cerberus.approvement.dto.ApprovementSimpleDto;
import cz.bbn.cerberus.approvement.enums.ApprovementState;
import cz.bbn.cerberus.approvement.enums.ApprovementType;
import cz.bbn.cerberus.approvement.enums.BusinessTripTransportationType;
import cz.bbn.cerberus.approvement.factory.ApprovementFactory;
import cz.bbn.cerberus.approvement.persistance.dao.ApprovementDao;
import cz.bbn.cerberus.approvement.persistance.entity.ApprovementBusinessTripEntity;
import cz.bbn.cerberus.approvement.persistance.entity.ApprovementDayEntity;
import cz.bbn.cerberus.approvement.persistance.entity.ApprovementEntity;
import cz.bbn.cerberus.approvement.persistance.entity.ApprovementProjectEmployeeEntity;
import cz.bbn.cerberus.approvement.persistance.entity.ApprovementProjectEmployeeId;
import cz.bbn.cerberus.approvement.persistance.repository.ApprovementBusinessTripRepository;
import cz.bbn.cerberus.approvement.persistance.repository.ApprovementDayRepository;
import cz.bbn.cerberus.approvement.persistance.repository.ApprovementProjectEmployeeRepository;
import cz.bbn.cerberus.approvement.persistance.repository.ApprovementRepository;
import cz.bbn.cerberus.approvement.ui.ApprovementApproverView;
import cz.bbn.cerberus.approvement.ui.ApprovementBusinessTripView;
import cz.bbn.cerberus.approvement.ui.ApprovementHolidayView;
import cz.bbn.cerberus.approvement.ui.ApprovementHomeOfficeView;
import cz.bbn.cerberus.approvement.ui.ApprovementPaidLeaveView;
import cz.bbn.cerberus.approvement.ui.ApprovementUnpaidLeaveView;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.employee.persistance.entity.EmployeeEntity;
import cz.bbn.cerberus.holiday.persistance.HolidayEntity;
import cz.bbn.cerberus.notification.NotificationService;
import cz.bbn.cerberus.project.persistance.entity.ProjectEntity;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.usermessage.MessageType;
import cz.bbn.cerberus.workreport.WorkReportService;
import cz.bbn.cerberus.workreport.dto.WorkReportDto;
import cz.bbn.cerberus.workreport.dto.WorkReportPickDto;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

@Service
public class ApprovementService {

    private final ApprovementDao approvementDao;
    private final ApprovementRepository approvementRepository;
    private final ApprovementProjectEmployeeRepository approvementProjectEmployeeRepository;
    private final AppLogService appLogService;
    private final WorkReportService workReportService;
    private final ApprovementBusinessTripRepository approvementBusinessTripRepository;
    private final UserService userService;
    private final NotificationService notificationService;
    private final AppEnv appEnv;
    private final ApprovementDayRepository approvementDayRepository;

    public ApprovementService(ApprovementDao approvementDao, ApprovementRepository approvementRepository,
                              ApprovementProjectEmployeeRepository approvementProjectEmployeeRepository,
                              AppLogService appLogService, WorkReportService workReportService,
                              ApprovementBusinessTripRepository approvementBusinessTripRepository,
                              UserService userService, NotificationService notificationService, AppEnv appEnv,
                              ApprovementDayRepository approvementDayRepository) {
        this.approvementDao = approvementDao;
        this.approvementRepository = approvementRepository;
        this.approvementProjectEmployeeRepository = approvementProjectEmployeeRepository;
        this.appLogService = appLogService;
        this.workReportService = workReportService;
        this.approvementBusinessTripRepository = approvementBusinessTripRepository;
        this.userService = userService;
        this.notificationService = notificationService;
        this.appEnv = appEnv;
        this.approvementDayRepository = approvementDayRepository;
    }

    public Page<ApprovementSimpleDto> findApprovementDtoPage(ApprovementFilterDto filter) {
        return approvementDao.findApprovementDtoPage(filter);
    }

    public ApprovementDto getApprovementDto(Long id) throws SystemException {
        ApprovementEntity entity = getEntityById(id);
        return ApprovementFactory.fromEntity(entity);
    }

    public Double getCountByType(ApprovementType type, Integer year) {
        return approvementDayRepository.getCount(type, year, SecurityUtils.getCurrentUserId()).orElse(0D) + (
                approvementDayRepository.getCountHalfDays(
                        type, year, SecurityUtils.getCurrentUserId()).orElse(0D) * 0.5);
    }

    public Double getCountByType(ApprovementType type, Integer year, Long userId) {
        return approvementDayRepository.getCount(type, year, userId).orElse(0D);
    }

    public ApprovementDayEntity getApprovementByDate(LocalDate from, LocalDate to, Long id, Long userId) {
        return approvementDayRepository.getApprovementByDate(from.atStartOfDay(), to.atTime(LocalTime.MAX), userId, id, PageRequest.of(0, 1));
    }

    public ApprovementDayEntity getApprovementByDate(LocalDate from, LocalDate to, Long userId) {
        return approvementDayRepository.getApprovementByDate(from.atStartOfDay(), to.atTime(LocalTime.MAX), userId, PageRequest.of(0, 1));
    }

    @Transactional
    public Long saveApprovement(ApprovementDto dto, List<String[]> projectList,
                                List<HolidayEntity> holidayEntityList, boolean master) throws SystemException {
        ApprovementEntity entity = new ApprovementEntity();
        Long id = saveApprovement(entity, dto, holidayEntityList, projectList);
        dto.setId(id);
        saveApprovementProjectEmployee(id, dto.getOwnerUserDto().getEmployee(), projectList, dto.getApprovementType());
        appLogService.log(master ? "Master new approvement request" : "New approvement request",
                getNewOrChangedApprovementMessage(dto, projectList, false), String.valueOf(dto.getId()));
        changeStatusAndSaveMails(dto, null, master);
        return id;
    }

    private String getNewOrChangedApprovementMessage(ApprovementDto dto, List<String[]> projectList, boolean update) {
        String message = "id=".concat(String.valueOf(dto.getId()))
                .concat(", type=").concat(dto.getApprovementType().name())
                .concat(", state=").concat(dto.getApprovementState().name())
                .concat(", from=").concat(dto.getDateFrom().toString())
                .concat(", to=").concat(dto.getDateTo().toString())
                .concat(", days=").concat(dto.getDays().toString())
                .concat(", lineManager=").concat(dto.getLineManagerUserDto() == null ? "" :
                        String.valueOf(dto.getLineManagerUserDto().getId()).concat(" - ").concat(dto.getLineManagerUserDto().getName()))
                .concat(", superior=").concat(dto.getSuperiorUserDto() == null ? "" : String.valueOf(dto.getSuperiorUserDto().getId()).concat(" - ").concat(dto.getSuperiorUserDto().getName()))
                .concat(", lineManagerRole=".concat(dto.getLineManagerRole() == null ? "" : dto.getLineManagerRole().getId()))
                .concat(", createdBy=").concat(dto.getCreatedUserDto() == null ? "" : String.valueOf(dto.getCreatedUserDto().getId()).concat(" - ").concat(dto.getCreatedUserDto().getName()))
                .concat(", owner=").concat(String.valueOf(dto.getOwnerUserDto().getId()).concat(" - ").concat(dto.getOwnerUserDto().getName()))
                .concat(", created=").concat(dto.getCreated().toString());

        if (update) {
            message = message.concat(", lineManagerApproved=").concat(dto.getLineManageApproved().toString())
                    .concat(", lineManagerApprovedDate=").concat(dto.getLineManageApprovedDate() == null ? "" :
                            dto.getLineManageApprovedDate().toString())
                    .concat(", lineManagerNote=").concat(dto.getLineManageNote())
                    .concat(", superiorNote=").concat(dto.getSuperiorNote())
                    .concat(", superiorApprovedDate=").concat(dto.getSuperiorApprovedDate() == null ? "" :
                            dto.getSuperiorApprovedDate().toString())
                    .concat(", superiorApproved=").concat(dto.getSuperiorApproved().toString());
        }

        if (dto.getApprovementBusinessTripDto() != null) {
            ApprovementBusinessTripDto tripDto = dto.getApprovementBusinessTripDto();
            message = message.concat(", approvementFromSubject=").concat(tripDto.getApprovementFromSubjectDto() == null ? "" : tripDto.getApprovementFromSubjectDto().getId())
                    .concat(", approvementToSubject=").concat(tripDto.getApprovementToSubjectDto() == null ? "" :
                            (tripDto.getApprovementToSubjectDto().getId() == null ? "" : tripDto.getApprovementToSubjectDto().getId()))
                    .concat(", approvementToAnother=").concat(tripDto.getApprovementToAnother())
                    .concat(", purpose=").concat(String.valueOf(tripDto.getPurposeDto().getId()))
                    .concat(", interruptionFrom=").concat(tripDto.getInterruptionFrom().toString())
                    .concat(", interuptionTo=").concat(tripDto.getInterruptionTo().toString())
                    .concat(", project=").concat(tripDto.getProjectDto() == null ? "" : tripDto.getProjectDto().getId())
                    .concat(", opportunity").concat(tripDto.getOpportunityDto() == null ? "" : tripDto.getOpportunityDto().getId());
            if (tripDto.getBusinessTripTransportationTypeSet() != null) {
                message = message.concat(", transportationType=");
                for (BusinessTripTransportationType transportationType :
                        tripDto.getBusinessTripTransportationTypeSet()) {
                    message = message.concat(transportationType.name()).concat(", ");
                }
            }
            if (tripDto.getFellowPassengers() != null) {
                message = message.concat(", fellowPassengers=");
                for (EmployeeDto employeeDto : tripDto.getFellowPassengers()) {
                    message = message.concat(employeeDto.getId()).concat(", ");
                }
            }
        }

        AtomicReference<String> projects = new AtomicReference<>("");
        AtomicReference<String> employees = new AtomicReference<>("");
        projectList.forEach(strings -> {
            if (strings[0] != null) {
                projects.set(projects.get().concat(strings[0]).concat(", "));
            }
            if (strings[1] != null) {
                employees.set(employees.get().concat(strings[1]).concat(", "));
            }
        });
        if (!projects.get().isEmpty()) {
            message = message.concat("projects=").concat(projects.get());
        }
        if (!employees.get().isEmpty()) {
            message = message.concat("employees=").concat(employees.get());
        } else if (dto.getApprovementType() == ApprovementType.ILL) {
            message = message.concat("employee=").concat(dto.getFirstEmployeeDto().getId());
        }
        message = message.concat(", note=").concat(dto.getNote());
        return message;
    }

    @Transactional
    public Long updateApprovement(ApprovementDto dto, ApprovementDto originalDto, List<String[]> projectList,
                                  List<HolidayEntity> holidayEntityList, boolean master) {
        ApprovementEntity entity = new ApprovementEntity();
        Long id = saveApprovement(entity, dto, holidayEntityList, projectList);
        saveApprovementProjectEmployee(id, dto.getOwnerUserDto().getEmployee(), projectList, dto.getApprovementType());
        appLogService.log(master ? "Master approvement updated request" : "Approvement updated request",
                getNewOrChangedApprovementMessage(dto, projectList, true), String.valueOf(dto.getId()));
        changeStatusAndSaveMails(dto, originalDto, master);
        return id;
    }

    private void changeStatusAndSaveMails(ApprovementDto dto, ApprovementDto originalDto, boolean master) {
        if(master){
            LocalDate from = dto.getDateFrom();
            saveMasterEmailNotification(dto, originalDto);
            if(originalDto != null && LocalDate.now().isBefore(from) && originalDto.getApprovementState().equals(dto.getApprovementState())) {
                if (dto.isLMSuperiorEquals() || Boolean.TRUE.equals(dto.getLineManageApproved())) {
                    approvementRepository.updateState(ApprovementState.WAITING_FOR_SUPERIOR, dto.getId());
                } else {
                    approvementRepository.updateState(ApprovementState.WAIT_FOR_LM_MANAGER, dto.getId());
                }
            } else if(originalDto != null){
                dto.setSuperiorUserDto(SecurityUtils.getCurrentUserDto());
                approvementRepository.updateSuperior(SecurityUtils.getCurrentUserDto().getId(), dto.getId());
            }
        }else if(originalDto != null){
            saveEmailNotification(dto, originalDto);
        }
    }

    private void generateApprovementDays(ApprovementDto dto, List<HolidayEntity> holidayEntityList) {
        LocalDateTime dateFrom = dto.getDateFrom().atStartOfDay();
        LocalDateTime dateTo = dto.getDateTo().atStartOfDay();
        approvementDayRepository.deleteByApprovementEntityId(dto.getId());
        List<ApprovementDayEntity> approvementEntityList = new ArrayList<>();
        while (dateFrom.isBefore(dateTo) || dateFrom.isEqual(dateTo)) {
            if (!holidayEntityList.contains(new HolidayEntity(dateFrom.toLocalDate()))
                    && dateFrom.getDayOfWeek() != DayOfWeek.SATURDAY
                    && dateFrom.getDayOfWeek() != DayOfWeek.SUNDAY) {
                ApprovementDayEntity approvementDayEntity = new ApprovementDayEntity();
                ApprovementEntity approvementEntity = new ApprovementEntity();
                approvementEntity.setId(dto.getId());
                approvementDayEntity.setApprovementEntity(approvementEntity);
                approvementDayEntity.setDate(dateFrom);
                approvementEntityList.add(approvementDayEntity);
            }
            dateFrom = dateFrom.plusDays(1);
        }
        approvementDayRepository.saveAll(approvementEntityList);
    }

    private void saveMasterEmailNotification(ApprovementDto dto, ApprovementDto originalDto) {
        String title = "";
        Map<Long, String> userIdMap = new HashMap<>();
        if (dto.getApprovementType() == ApprovementType.ILL) {
            return;
        }
        title = dto.getId() == null ? MessageType.MASTER_NEW_APPROVEMENT_REQUEST.name() :
                MessageType.MASTER_CHANGED_APPROVEMENT_REQUEST.name();
        if (dto.getApprovementType() == ApprovementType.PAID_LEAVE) {
            Set<Long> userIdList = userService.findUserIdListByBoRole();
            userIdList.forEach(aLong -> userIdMap.put(aLong, getApprovementLink(dto)));
        } else if (dto.getLineManagerUserDto() != null) {
            userIdMap.put(dto.getLineManagerUserDto().getId(), getApprovementLink(dto));
        }
        userIdMap.put(dto.getSuperiorUserDto().getId(), getCreatorLink(dto));
        userIdMap.put(dto.getOwnerUserDto().getId(), getCreatorLink(dto));

        for (Map.Entry<Long, String> entry : userIdMap.entrySet()) {
            String message = getEmailMessage(dto, entry.getValue());
            notificationService.saveEmailHighNotification(title, message, entry.getKey());
        }
    }

    private void saveEmailNotification(ApprovementDto dto, ApprovementDto originalDto) {
        if (dto.getApprovementType() == ApprovementType.ILL) {
            return;
        }
        String title = "";
        Map<Long, String> userIdMap = new HashMap<>();
        if (originalDto == null) {
            title = MessageType.NEW_APPROVEMENT_REQUEST.name();
            if (dto.getApprovementType() == ApprovementType.PAID_LEAVE) {
                Set<Long> userIdList = userService.findUserIdListByBoRole();
                userIdList.forEach(aLong -> userIdMap.put(aLong, getApprovementLink(dto)));
            } else {
                userIdMap.put(dto.getApprovementState() == ApprovementState.WAIT_FOR_LM_MANAGER ?
                                dto.getLineManagerUserDto().getId() : dto.getSuperiorUserDto().getId()
                        , getApprovementLink(dto));
            }
        } else {
            switch (dto.getApprovementState()) {
                case WAIT_FOR_LM_MANAGER -> {
                    title = MessageType.NEW_APPROVEMENT_REQUEST.name();
                    if (dto.getApprovementType() == ApprovementType.PAID_LEAVE) {
                        Set<Long> userIdList = userService.findUserIdListByBoRole();
                        userIdList.forEach(aLong -> userIdMap.put(aLong, getApprovementLink(dto)));
                    } else if (dto.getLineManagerUserDto() != null) {
                        userIdMap.put(dto.getLineManagerUserDto().getId(), getApprovementLink(dto));
                    }
                }
                case WAITING_FOR_SUPERIOR -> {
                    title = MessageType.NEW_APPROVEMENT_REQUEST.name();
                    userIdMap.put(dto.getSuperiorUserDto().getId(), getCreatorLink(dto));
                }
                case RETURNED_FOR_COMPLETION -> {
                    title = MessageType.APPROVEMENT_REQUEST_RETURNED.name();
                    userIdMap.put(dto.getOwnerUserDto().getId(), getCreatorLink(dto));
                    if (dto.getLineManagerUserDto() != null
                            && !dto.getLineManagerUserDto().getId().equals(dto.getSuperiorUserDto().getId())) {
                        userIdMap.put(dto.getLineManagerUserDto().getId(), getApprovementLink(dto));
                    } else if (dto.getApprovementType() == ApprovementType.PAID_LEAVE) {
                        Set<Long> userIdList = userService.findUserIdListByBoRole();
                        userIdList.forEach(aLong -> userIdMap.put(aLong, getApprovementLink(dto)));
                    }
                }
                case COMPLETELY_APPROVED -> {
                    title = MessageType.APPROVEMENT_REQUEST_APPROVED.name();
                    userIdMap.put(dto.getOwnerUserDto().getId(), getCreatorLink(dto));
                }
                case DENIED -> {
                    title = MessageType.APPROVEMENT_REQUEST_DENIED.name();
                    userIdMap.put(dto.getOwnerUserDto().getId(), getCreatorLink(dto));
                    if (originalDto.getApprovementState() == ApprovementState.WAITING_FOR_SUPERIOR) {
                        if (dto.getLineManagerUserDto() != null
                                && !dto.getLineManagerUserDto().getId().equals(dto.getSuperiorUserDto().getId())) {
                            userIdMap.put(dto.getLineManagerUserDto().getId(), getApprovementLink(dto));
                        } else if (dto.getApprovementType() == ApprovementType.PAID_LEAVE) {
                            Set<Long> userIdList = userService.findUserIdListByBoRole();
                            userIdList.forEach(aLong -> userIdMap.put(aLong, getApprovementLink(dto)));
                        }
                    }
                }
            }
        }

        for (Map.Entry<Long, String> entry : userIdMap.entrySet()) {
            String message = getEmailMessage(dto, entry.getValue());
            notificationService.saveEmailHighNotification(title, message, entry.getKey());
        }
    }

    private String getApprovementLink(ApprovementDto dto) {
        return AppUtils.generateUrl(appEnv.getProjectUrl(), ApprovementApproverView.ROUTE,
                "&id=" + dto.getId(), Transl.get("Click to link"));
    }

    private String getCreatorLink(ApprovementDto dto) {
        String route = "";
        switch (dto.getApprovementType()) {
            case HOLIDAY -> route = ApprovementHolidayView.ROUTE;
            case PAID_LEAVE -> route = ApprovementPaidLeaveView.ROUTE;
            case HOME_OFFICE -> route = ApprovementHomeOfficeView.ROUTE;
            case UNPAID_LEAVE -> route = ApprovementUnpaidLeaveView.ROUTE;
            case BUSSINES_TRIP -> route = ApprovementBusinessTripView.ROUTE;
        }
        return AppUtils.generateUrl(appEnv.getProjectUrl(), route, "&id=" + dto.getId(), Transl.get("Click to link"));
    }

    private String getEmailMessage(ApprovementDto dto, String link) {
        String lineManagerName = "";
        if (dto.getLineManagerUserDto() != null) {
            lineManagerName = dto.getLineManagerUserDto().getName();
        }
        String creatorOwnerText = "";
        if(dto.getCreatedUserDto().getId() != dto.getOwnerUserDto().getId()){
            creatorOwnerText = creatorOwnerText.concat(Transl.get("Creator")).concat(": ").concat(dto.getCreatedUserDto().getName())
                    .concat("</b><br /><br />")
                    .concat(Transl.get("Created for user")).concat(": ").concat(dto.getOwnerUserDto().getName());
        }else{
            creatorOwnerText = creatorOwnerText.concat(Transl.get("Creator")).concat(": ").concat(dto.getOwnerUserDto().getName());
        }

        return "<br />".concat(Transl.get("Request type")).concat(": ").concat("<b>")
                .concat(Transl.get(dto.getApprovementType().name()))
                .concat("</b><br /><br />")
                .concat(Transl.get("Date from")).concat(": ").concat(AppUtils.formatDate(dto.getDateFrom()))
                .concat("<br /><br />")
                .concat(Transl.get("Date to")).concat(": ").concat(AppUtils.formatDate(dto.getDateTo()))
                .concat("<br /><br />")
                .concat(Transl.get("Days")).concat(": ")
                .concat(Boolean.TRUE.equals(dto.getHalfDay()) ? Transl.get("Half day") : dto.getDays().toString())
                .concat("<br /><br />")
                .concat(Transl.get("Actual state")).concat(": ").concat("<b>")
                .concat(Transl.get(dto.getApprovementState().name())).concat("</b>")
                .concat("<br /><br />")
                .concat(creatorOwnerText)
                .concat("<br /><br />")
                .concat(Transl.get("Line manager")).concat(": ")
                .concat(dto.getApprovementType() != ApprovementType.PAID_LEAVE
                        ? lineManagerName : Transl.get("Back office"))
                .concat("<br /><br />")
                .concat(Transl.get("Superior")).concat(": ").concat(dto.getSuperiorUserDto().getName())
                .concat("<br /><br />")
                .concat(Transl.get("Created")).concat(": ").concat(AppUtils.formatDateTime(dto.getCreated(), true))
                .concat("<br /><br />")
                .concat(Transl.get("Action created")).concat(": ")
                .concat(AppUtils.formatDateTime(LocalDateTime.now(), true))
                .concat("<br /><br />")
                .concat(link);
    }

    public int userApprovementCount(Long userId, List<ApprovementType> approvementTypeList, LocalDate date) {
        return approvementDayRepository.getUserApprovementCount(userId, approvementTypeList, date.atStartOfDay());
    }


    private void saveWorkReport(ApprovementDto dto, List<HolidayEntity> holidayEntityList, List<String[]> projectList) {
        workReportService.deleteWorkReport(dto.getId(),
                dto.getOwnerUserDto().getEmployee().getId());
        LocalDate dateFrom = dto.getDateFrom();
        int allDays = (int) ChronoUnit.DAYS.between(dateFrom, dto.getDateTo()) + 1;
        for (int i = 0; i < allDays; i++) {
            if (dateFrom.getDayOfWeek() != DayOfWeek.SATURDAY && dateFrom.getDayOfWeek() != DayOfWeek.SUNDAY
                    && !holidayEntityList.contains(new HolidayEntity(dateFrom))) {
                WorkReportDto workReportDto = new WorkReportDto();
                workReportDto.setApprovementDto(dto);
                if (Boolean.TRUE.equals(dto.getHalfDay()) && dto.getApprovementType() != ApprovementType.HOME_OFFICE
                        && dto.getApprovementType() != ApprovementType.BUSSINES_TRIP) {
                    workReportDto.setDuration(4D);
                } else if (dto.getApprovementType() != ApprovementType.HOME_OFFICE
                        && dto.getApprovementType() != ApprovementType.BUSSINES_TRIP) {
                    workReportDto.setDuration(8D);
                }
                workReportDto.setDate(dateFrom);
                workReportDto.setEmployeeId(dto.getOwnerUserDto().getEmployee().getId());
                WorkReportPickDto workReportPickDto = new WorkReportPickDto();
                if (!projectList.isEmpty()) {
                    workReportPickDto.setId(projectList.get(0)[0]);
                    workReportDto.setItemId(projectList.get(0)[0]);
                }
                workReportDto.setPickDto(workReportPickDto);
                workReportService.save(workReportDto);
            }
            dateFrom = dateFrom.plusDays(1);
        }
    }

    public void saveApprovementProjectEmployee(Long id, EmployeeDto employeeDto, List<String[]> projectList,
                                               ApprovementType approvementType) {
        Set<ApprovementProjectEmployeeEntity> oldSet =
                approvementProjectEmployeeRepository.findByApprovementId(id);
        List<ApprovementProjectEmployeeEntity> entityList = new ArrayList<>();
        projectList.forEach(array -> {
            ApprovementProjectEmployeeEntity approvementProjectEmployeeEntity =
                    new ApprovementProjectEmployeeEntity(new ApprovementProjectEmployeeId());
            approvementProjectEmployeeEntity.getId().setApprovementId(id);
            ProjectEntity projectEntity = new ProjectEntity();
            if (array[0] != null) {
                projectEntity.setId(array[0]);
            }
            approvementProjectEmployeeEntity.getId().setProjectEntity(projectEntity);
            EmployeeEntity employeeEntity = new EmployeeEntity();
            if (approvementType.isEmployeeMandatory() && array[0] != null) {
                employeeEntity.setId(array[1]);
            } else if (approvementType != ApprovementType.HOLIDAY) {
                employeeEntity.setId(employeeDto.getId());
            }
            approvementProjectEmployeeEntity.getId().setEmployeeEntity(employeeEntity);
            if (projectEntity.getId() != null && employeeEntity.getId() != null) {
                entityList.add(approvementProjectEmployeeEntity);
            }
        });
        Set<ApprovementProjectEmployeeEntity> deleteSet = new HashSet<>();
        Set<ApprovementProjectEmployeeEntity> newSet = new HashSet<>();
        for (ApprovementProjectEmployeeEntity appEntity : oldSet) {
            boolean contain = false;
            for (ApprovementProjectEmployeeEntity newEntity : entityList) {
                if (Objects.equals(newEntity.getId().getApprovementId(), appEntity.getId().getApprovementId())
                        && Objects.equals(newEntity.getId().getProjectEntity().getId(),
                        appEntity.getId().getProjectEntity().getId())
                        && Objects.equals(newEntity.getId().getEmployeeEntity().getId(),
                        appEntity.getId().getEmployeeEntity().getId())) {
                    contain = true;
                    break;
                }
            }
            if (!contain) {
                deleteSet.add(appEntity);
            }
        }
        for (ApprovementProjectEmployeeEntity newEntity : entityList) {
            boolean contain = false;
            for (ApprovementProjectEmployeeEntity appEntity : oldSet) {
                if (Objects.equals(newEntity.getId().getApprovementId(), appEntity.getId().getApprovementId())
                        && Objects.equals(newEntity.getId().getProjectEntity().getId(),
                        appEntity.getId().getProjectEntity().getId())
                        && Objects.equals(newEntity.getId().getEmployeeEntity().getId(),
                        appEntity.getId().getEmployeeEntity().getId())) {
                    contain = true;
                    break;
                }
            }
            if (!contain) {
                newSet.add(newEntity);
            }
        }
        approvementProjectEmployeeRepository.deleteAll(deleteSet);
        approvementProjectEmployeeRepository.saveAll(newSet);
    }

    private Long saveApprovement(ApprovementEntity entity, ApprovementDto dto, List<HolidayEntity> holidayEntityList,
                                 List<String[]> projectList) {
        ApprovementFactory.fillEntity(entity, dto);
        if (dto.getApprovementType() == ApprovementType.BUSSINES_TRIP) {
            ApprovementBusinessTripEntity approvementBusinessTripEntity = new ApprovementBusinessTripEntity();
            ApprovementFactory.fillEntity(approvementBusinessTripEntity, dto.getApprovementBusinessTripDto());
            Long id = approvementBusinessTripRepository.save(approvementBusinessTripEntity).getId();
            approvementBusinessTripEntity.setId(id);
            entity.setApprovementBusinessTripEntity(approvementBusinessTripEntity);
        }
        Long id = approvementRepository.save(entity).getId();
        dto.setId(id);
        if (dto.getApprovementState() != ApprovementState.CANCELED
                && dto.getApprovementState() != ApprovementState.DENIED) {
            saveWorkReport(dto, holidayEntityList, projectList);
        } else {
            workReportService.deleteWorkReport(dto.getId(),
                    dto.getOwnerUserDto().getEmployee().getId());
        }
        generateApprovementDays(dto, holidayEntityList);
        return id;
    }

    private ApprovementEntity getEntityById(Long id) throws SystemException {
        return approvementRepository.getApprovementEntity(id)
                .orElseThrow(() -> new SystemException(ErrorCode.APPROVEMENT_NOT_EXITS, id));
    }
}
