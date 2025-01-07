package cz.bbn.cerberus.task;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.notification.NotificationService;
import cz.bbn.cerberus.role.factory.RoleFactory;
import cz.bbn.cerberus.role.persistance.entity.RoleEntity;
import cz.bbn.cerberus.task.dto.SendTask;
import cz.bbn.cerberus.task.dto.TaskCheckDto;
import cz.bbn.cerberus.task.dto.TaskDto;
import cz.bbn.cerberus.task.dto.TaskEntityType;
import cz.bbn.cerberus.task.dto.TaskFilterDto;
import cz.bbn.cerberus.task.dto.TaskState;
import cz.bbn.cerberus.task.factory.TaskCheckFactory;
import cz.bbn.cerberus.task.factory.TaskFactory;
import cz.bbn.cerberus.task.persitance.entity.TaskCheckEntity;
import cz.bbn.cerberus.task.persitance.entity.TaskEntity;
import cz.bbn.cerberus.task.persitance.entity.TaskRoleEntity;
import cz.bbn.cerberus.task.persitance.entity.TaskRoleId;
import cz.bbn.cerberus.task.persitance.entity.TaskUserEntity;
import cz.bbn.cerberus.task.persitance.entity.TaskUserId;
import cz.bbn.cerberus.task.persitance.repository.TaskByRoleRepository;
import cz.bbn.cerberus.task.persitance.repository.TaskByUserRepository;
import cz.bbn.cerberus.task.persitance.repository.TaskCheckRepository;
import cz.bbn.cerberus.task.persitance.repository.TaskDao;
import cz.bbn.cerberus.task.persitance.repository.TaskRepository;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;
import cz.bbn.cerberus.usermessage.MessageType;
import org.apache.commons.lang3.SerializationUtils;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class TaskService {

    private final TaskDao taskDao;
    private final TaskRepository taskRepository;
    private final TaskByUserRepository taskByUserRepository;
    private final TaskByRoleRepository taskByRoleRepository;
    private final TaskCheckRepository taskCheckRepository;
    private final AppLogService appLogService;
    private final NotificationService notificationService;
    private final ListService listService;
    private final AppEnv appEnv;

    public TaskService(TaskDao taskDao, TaskRepository taskRepository,
                       TaskByUserRepository taskByUserRepository, TaskByRoleRepository taskByRoleRepository,
                       TaskCheckRepository taskCheckRepository, AppLogService appLogService,
                       NotificationService notificationService, ListService listService, AppEnv appEnv) {
        this.taskDao = taskDao;
        this.taskRepository = taskRepository;
        this.taskByUserRepository = taskByUserRepository;
        this.taskByRoleRepository = taskByRoleRepository;
        this.taskCheckRepository = taskCheckRepository;
        this.appLogService = appLogService;
        this.notificationService = notificationService;
        this.listService = listService;
        this.appEnv = appEnv;
    }

    public Page<TaskDto> findTaskDtoPage(TaskFilterDto filter) {
        return taskDao.findTaskDtoPage(filter);
    }

    public int getTaskCount(TaskFilterDto filter) {
        return taskDao.getTaskCount(filter);
    }

    public Long getCountUserTasks(Long userId, LocalDateTime from, LocalDateTime to) {
        return taskRepository.countByUserAndDate(userId, from, to) +
                taskRepository.countByTaskUserEntityAndDate(userId, from, to);
    }

    public List<TaskDto> findTaskEntityByUser(LocalDateTime start, LocalDateTime end) {
        Long id = SecurityUtils.getCurrentUserId();
        List<TaskEntity> finalTaskList =
                new ArrayList<>(taskRepository.findByTaskUserEntitySetIdUserEntity(id, start, end));
        return ConvertEntities
                .fromEntities(finalTaskList, TaskFactory::fromEntity);
    }

    @Transactional
    public TaskDto getTaskDto(Long id) throws SystemException {
        TaskEntity taskEntity = getTaskEntity(id);
        return TaskFactory.fromEntity(taskEntity, true);
    }

    @Transactional
    public void saveTask(TaskDto dto) {
        TaskEntity entity = new TaskEntity();
        saveTask(entity, dto);
        appLogService.logInsert(dto, DomainEnum.TASK_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void updateTask(TaskDto dto, TaskDto originalDto) throws SystemException {
        TaskEntity entity = getTaskEntity(dto.getId());
        saveTask(entity, dto);
        checkAndSendNotifications(dto, originalDto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.TASK_DOMAIN_NAME.getValue());
    }

    private void checkAndSendNotifications(TaskDto dto, TaskDto originalDto) {
        checkTaskUpdate(dto);
        checkUpdateStatus(dto, originalDto);
        checkTaskResolved(dto, originalDto);
        checkAssigneeChanged(dto, originalDto);
        checkTaskCheckList(dto, originalDto);
    }

    private void checkTaskCheckList(TaskDto dto, TaskDto originalDto) {
        LocalDateTime date = dto.getDate().minusDays(dto.getDaysToNotify());
        dto.getTaskCheckDtoList().forEach(taskCheckDto -> {
            Set<UserDto> statusChangeUseSet = new HashSet<>();
            if (taskCheckDto.getCompleteDate().isBefore(date)) {
                TaskCheckDto actualOriginalCheckDto = null;
                if (originalDto.getTaskCheckDtoList() != null) {
                    actualOriginalCheckDto = originalDto.getTaskCheckDtoList().stream().filter(originalCheckDto ->
                            originalCheckDto.getId().equals(taskCheckDto.getId())).findFirst().orElse(null);
                }
                if (actualOriginalCheckDto == null || (actualOriginalCheckDto.getValue().equals(Boolean.FALSE) && taskCheckDto.getValue().equals(Boolean.TRUE))) {
                    if (dto.getUserDto().getId() != SecurityUtils.getCurrentUserId()) {
                        statusChangeUseSet.add(dto.getUserDto());
                    }
                    statusChangeUseSet.addAll(getResolverUserList(dto));

                    statusChangeUseSet.forEach(actualUserDto -> {
                        String language = Optional.ofNullable(actualUserDto.getPreferredLanguage()).orElse(Transl.DEFAULT_LANG);
                        String message = getCheckListMessage(taskCheckDto, dto, language);
                        sendBySendTask(dto, message, actualUserDto, MessageType.TASK_CHECK_LIST_DATE);
                    });
                }
            }
        });
    }

    private void checkTaskUpdate(TaskDto dto) {
        if (dto.getAssignee() != null && !dto.getAssignee().getId().equals(SecurityUtils.getCurrentUserId())) {
            String language = Optional.ofNullable(dto.getUserDto().getPreferredLanguage()).orElse(Transl.DEFAULT_LANG);
            String message = getTaskUpdatedMessage(dto, language);
            sendBySendTask(dto, message, dto.getAssignee(), MessageType.TASK_UPDATED);
        }
    }

    private void checkTaskResolved(TaskDto dto, TaskDto originalDto) {
        if (!dto.getState().equals(originalDto.getState()) && dto.getState().equals(TaskState.RESOLVED)
                && dto.getUserDto().getId() != SecurityUtils.getCurrentUserId()) {
            String language = Optional.ofNullable(dto.getUserDto().getPreferredLanguage()).orElse(Transl.DEFAULT_LANG);
            String message = getTaskResolvedMessage(dto, language);
            sendBySendTask(dto, message, dto.getUserDto(), MessageType.TASK_RESOLVED);
        }
    }

    private void checkUpdateStatus(TaskDto dto, TaskDto originalDto) {
        UserDto userDto = dto.getUserDto();
        Set<UserDto> statusChangeUseSet = new HashSet<>();
        UserDto currentUserDto = SecurityUtils.getCurrentUserDto();
        if (!dto.getState().equals(originalDto.getState())) {
            if (userDto.getId() != currentUserDto.getId()) {
                statusChangeUseSet.add(userDto);
            }
            statusChangeUseSet.addAll(getResolverUserList(dto));
        }

        statusChangeUseSet.forEach(actualUserDto -> {
            String language = Optional.ofNullable(actualUserDto.getPreferredLanguage()).orElse(Transl.DEFAULT_LANG);
            String message = getStateChangeMessage(dto, originalDto, language);
            sendBySendTask(dto, message, actualUserDto, MessageType.TASK_STATE_CHANGE);
        });
    }

    private void checkAssigneeChanged(TaskDto dto, TaskDto originalDto) {
        UserDto oldAssignee = Optional.ofNullable(originalDto.getAssignee()).orElse(new UserDto(0L));
        UserDto newAssignee = Optional.ofNullable(dto.getAssignee()).orElse(new UserDto(0L));
        if (!oldAssignee.getId().equals(newAssignee.getId())) {
            Set<UserDto> statusChangeUseSet = new HashSet<>();
            if (dto.getUserDto().getId() != SecurityUtils.getCurrentUser().getId()) {
                statusChangeUseSet.add(dto.getUserDto());
            }
            statusChangeUseSet.addAll(getResolverUserList(dto));

            statusChangeUseSet.forEach(actualUserDto -> {
                String language = Optional.ofNullable(actualUserDto.getPreferredLanguage()).orElse(Transl.DEFAULT_LANG);
                String message = getAssigneeChangeMessage(dto, originalDto, language);
                sendBySendTask(dto, message, actualUserDto, MessageType.TASK_ASSIGNEE_CHANGED);
            });
        }
    }

    private void sendBySendTask(TaskDto dto, String message, UserDto userDto, MessageType messageType) {
        dto.getSendTaskSet().forEach(sendTask -> {
            if (sendTask.equals(SendTask.EMAIL)) {
                notificationService.saveEmailHighNotification(messageType.name(), message, userDto.getId());
            } else if (sendTask.equals(SendTask.APP_NOTIFICATION)) {
                notificationService.saveAppNotificationHighNotification(messageType.name(), message, userDto.getId());
            } else {
                // posilani sms jeste neni implementovatno
            }
        });
    }

    public List<UserDto> getResolverUserList(TaskDto dto) {
        List<UserDto> userList = listService.getUserDtoList();
        Set<UserDto> userSet = new HashSet<>();
        if (dto.getRoleDtoSet() != null) {
            dto.getRoleDtoSet().forEach(roleDto ->
                    userList.stream().filter(actualUserDto -> actualUserDto.getUserRoles() != null
                                    && actualUserDto.getUserRoles().contains(roleDto.getId()))
                            .collect(Collectors.toList()).forEach(actualUserDto -> {
                                if (!actualUserDto.getId().equals(SecurityUtils.getCurrentUserId())) {
                                    userSet.add(actualUserDto);
                                }
                            }));
            dto.getUserDtoSet().forEach(actualUserDto -> {
                if (!actualUserDto.getId().equals(SecurityUtils.getCurrentUserId())) {
                    userSet.add(actualUserDto);
                }
            });
        }
        return userList;
    }

    private String getTaskResolvedMessage(TaskDto dto, String language) {
        String message = "";
        message = message.concat(Transl.getByLang("Task", language)).concat(": ")
                .concat(dto.getName()).concat(" ").concat(Transl.getByLang("Resolved", language)).concat("<br /><br />")
                .concat(Transl.getByLang("Changed by the user", language)).concat(": ").concat(SecurityUtils.getCurrentUserName()).concat("<br /><br />")
                .concat(Transl.getByLang("Date", language)).concat(": ").concat(AppUtils.formatDateTime(LocalDateTime.now(), true)).concat("<br /><br />")
                .concat(getTaskUrl(dto));
        return message;
    }

    private String getTaskUpdatedMessage(TaskDto dto, String language) {
        String message = "";
        message = message.concat(Transl.getByLang("Task", language)).concat(": ")
                .concat(dto.getName()).concat(" ").concat(Transl.getByLang("was updated", language)).concat("<br /><br />")
                .concat(Transl.getByLang("Changed by the user", language)).concat(": ").concat(SecurityUtils.getCurrentUserName()).concat("<br /><br />")
                .concat(Transl.getByLang("Date", language)).concat(": ").concat(AppUtils.formatDateTime(LocalDateTime.now(), true)).concat("<br /><br />")
                .concat(getTaskUrl(dto));
        return message;
    }

    private String getAssigneeChangeMessage(TaskDto dto, TaskDto originalDto, String language) {
        String message = "";
        message = message.concat(Transl.getByLang("Task", language)).concat(": ").concat(dto.getName()).concat("<br /><br />")
                .concat(Transl.getByLang("Old Assignee", language)).concat(": ")
                .concat(originalDto.getAssignee() == null ? "" : originalDto.getAssignee().getName()).concat("<br /><br />")
                .concat(Transl.getByLang("New assignee", language)).concat(": ")
                .concat(dto.getAssignee() == null ? "" : dto.getAssignee().getName()).concat("<br /><br />")
                .concat(Transl.getByLang("Changed by the user", language)).concat(": ").concat(SecurityUtils.getCurrentUserName()).concat("<br /><br />")
                .concat(Transl.getByLang("Date", language)).concat(": ").concat(AppUtils.formatDateTime(LocalDateTime.now(), true)).concat("<br /><br />")
                .concat(getTaskUrl(dto));
        return message;
    }

    private String getCheckListMessage(TaskCheckDto taskCheckDto, TaskDto dto, String language) {
        String message = "";
        message = message.concat(Transl.getByLang("Task", language)).concat(": ").concat(dto.getName()).concat("<br /><br />")
                .concat(Transl.getByLang("Checklist", language)).concat(": ")
                .concat(taskCheckDto.getCheckboxName()).concat(" ")
                .concat(Transl.getByLang("has an earlier date than event date", language)).concat(" ")
                .concat(AppUtils.formatDateTime(taskCheckDto.getCompleteDate(), true)).concat("<br /><br />")
                .concat(Transl.getByLang("Changed by the user", language)).concat(": ").concat(SecurityUtils.getCurrentUserName()).concat("<br /><br />")
                .concat(Transl.getByLang("Date", language)).concat(": ").concat(AppUtils.formatDateTime(LocalDateTime.now(), true)).concat("<br /><br />")
                .concat(getTaskUrl(dto));
        return message;
    }

    private String getStateChangeMessage(TaskDto dto, TaskDto originalDto, String language) {
        String message = "";
        message = message.concat(Transl.getByLang("Task", language)).concat(": ").concat(dto.getName()).concat("<br /><br />")
                .concat(Transl.getByLang("Old state", language)).concat(": ")
                .concat(Transl.getByLang(originalDto.getState().getTranslatedValue(), language)).concat("<br /><br />")
                .concat(Transl.getByLang("New state", language)).concat(": ")
                .concat(Transl.getByLang(dto.getState().getTranslatedValue(), language)).concat("<br /><br />")
                .concat(Transl.getByLang("Changed by the user", language)).concat(": ").concat(SecurityUtils.getCurrentUserName()).concat("<br /><br />")
                .concat(Transl.getByLang("Date", language)).concat(": ").concat(AppUtils.formatDateTime(LocalDateTime.now(), true)).concat("<br /><br />")
                .concat(getTaskUrl(dto));
        return message;
    }

    private String getTaskUrl(TaskDto taskD) {
        return "<a href='"
                .concat(appEnv.getProjectUrl())
                .concat("/")
                .concat("task-list/&id=")
                .concat(String.valueOf(taskD.getId()))
                .concat("'>")
                .concat(taskD.getName())
                .concat("</a>");
    }

    @Transactional
    public void deleteTask(Long id) throws SystemException {
        if (!taskRepository.existsById(id)) {
            throw new SystemException(ErrorCode.TASK_NOT_EXISTS, id);
        }
        TaskEntity entity = taskRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.TASK_NOT_EXISTS, id));
        TaskDto originalDto = TaskFactory.fromEntity(entity);
        TaskDto dto = SerializationUtils.clone(originalDto);
        dto.setDeleted(true);
        entity.setDeleted(true);
        taskRepository.save(entity);
        appLogService.logUpdate(dto, originalDto, DomainEnum.TASK_DOMAIN_NAME.getValue());
    }

    private void saveTask(TaskEntity entity, TaskDto dto) {
        if (dto.getCreationDate() == null) {
            dto.setCreationDate(LocalDateTime.now());
        }
        if (dto.getUserDto() == null) {
            dto.setUserDto(SecurityUtils.getCurrentUserDto());
        }
        TaskFactory.fillEntity(entity, dto);
        TaskEntity taskEntity = taskRepository.save(entity);
        Set<TaskUserEntity> taskUserEntitySet = new HashSet<>();
        dto.getUserDtoSet().forEach(userDto -> {
                    UserEntity userEntity = new UserEntity();
                    UserFactory.fillEntity(userEntity, userDto);
                    taskUserEntitySet.add(new TaskUserEntity(new TaskUserId(taskEntity, userEntity)));
                }
        );
        taskByUserRepository.deleteByIdTaskEntityId(taskEntity.getId());
        taskByUserRepository.saveAll(taskUserEntitySet);
        Set<TaskRoleEntity> taskRoleEntitySet = new HashSet<>();
        dto.getRoleDtoSet().forEach(roleDto -> {
                    RoleEntity roleEntity = new RoleEntity();
                    RoleFactory.fillEntity(roleEntity, roleDto);
                    taskRoleEntitySet.add(new TaskRoleEntity(new TaskRoleId(taskEntity, roleEntity)));
                }
        );
        taskByRoleRepository.deleteByIdTaskEntityId(taskEntity.getId());
        taskByRoleRepository.saveAll(taskRoleEntitySet);
        List<TaskCheckEntity> taskCheckEntityList = new ArrayList<>();
        dto.getTaskCheckDtoList().forEach(taskCheckDto -> {
            taskCheckDto.setEntityType(TaskEntityType.TASK.name());
            taskCheckDto.setEntityId(taskEntity.getId());
            TaskCheckEntity taskCheckEntity = new TaskCheckEntity();
            TaskCheckFactory.fillEntity(taskCheckEntity, taskCheckDto);
            taskCheckEntityList.add(taskCheckEntity);
        });
        taskCheckRepository.saveAll(taskCheckEntityList);
    }

    private TaskEntity getTaskEntity(Long id) throws SystemException {
        return taskRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.TASK_NOT_EXISTS, id));
    }

    public List<TaskCheckDto> getTaskCheckListByTaskId(Long id) {
        List<TaskCheckEntity> entityList = taskCheckRepository.findListByIdAndType(id, TaskEntityType.TASK.name());
        return ConvertEntities.fromEntities(entityList, TaskCheckFactory::fromEntity);
    }
}
