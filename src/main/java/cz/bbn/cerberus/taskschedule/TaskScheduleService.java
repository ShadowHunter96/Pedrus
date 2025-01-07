package cz.bbn.cerberus.taskschedule;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.role.factory.RoleFactory;
import cz.bbn.cerberus.role.persistance.entity.RoleEntity;
import cz.bbn.cerberus.task.dto.TaskCheckDto;
import cz.bbn.cerberus.task.dto.TaskEntityType;
import cz.bbn.cerberus.task.factory.TaskCheckFactory;
import cz.bbn.cerberus.task.persitance.entity.TaskCheckEntity;
import cz.bbn.cerberus.task.persitance.repository.TaskCheckRepository;
import cz.bbn.cerberus.taskschedule.dto.TaskScheduleDto;
import cz.bbn.cerberus.taskschedule.dto.TaskScheduleFilterDto;
import cz.bbn.cerberus.taskschedule.factory.TaskScheduleFactory;
import cz.bbn.cerberus.taskschedule.persistance.TaskScheduleDao;
import cz.bbn.cerberus.taskschedule.persistance.entity.TaskScheduleEntity;
import cz.bbn.cerberus.taskschedule.persistance.entity.TaskScheduleRoleEntity;
import cz.bbn.cerberus.taskschedule.persistance.entity.TaskScheduleRoleId;
import cz.bbn.cerberus.taskschedule.persistance.entity.TaskScheduleUserEntity;
import cz.bbn.cerberus.taskschedule.persistance.entity.TaskScheduleUserId;
import cz.bbn.cerberus.taskschedule.persistance.repository.TaskScheduleRepository;
import cz.bbn.cerberus.taskschedule.persistance.repository.TaskScheduleRoleRepository;
import cz.bbn.cerberus.taskschedule.persistance.repository.TaskScheduleUserRepository;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Service
public class TaskScheduleService {

    private final TaskScheduleRepository repository;
    private final TaskScheduleDao taskScheduleDao;
    private final TaskScheduleUserRepository taskScheduleUserRepository;
    private final TaskScheduleRoleRepository taskScheduleRoleRepository;
    private final TaskCheckRepository taskCheckRepository;
    private final AppLogService appLogService;

    public TaskScheduleService(TaskScheduleDao taskScheduleDao, TaskScheduleRepository repository,
                               TaskScheduleUserRepository taskScheduleUserRepository,
                               TaskScheduleRoleRepository taskScheduleRoleRepository,
                               TaskCheckRepository taskCheckRepository, AppLogService appLogService) {
        this.taskScheduleDao = taskScheduleDao;
        this.repository = repository;
        this.taskScheduleUserRepository = taskScheduleUserRepository;
        this.taskScheduleRoleRepository = taskScheduleRoleRepository;
        this.taskCheckRepository = taskCheckRepository;
        this.appLogService = appLogService;
    }


    public void deleteById(String id) throws SystemException {
        if (id != null && !id.isEmpty()) {
            TaskScheduleEntity entity = repository.findById(Long.parseLong(id))
                    .orElseThrow(() -> new SystemException(ErrorCode.TASK_SCHEDULE_NOT_EXISTS, id));
            entity.setDeleted(true);
            repository.save(entity);
        }
    }

    public Page<TaskScheduleDto> findTaskDtoPage(TaskScheduleFilterDto filter) {
        return taskScheduleDao.findTaskScheduleDtoPage(filter);
    }

    @Transactional
    public void saveSchedule(TaskScheduleDto dto) {
        TaskScheduleEntity entity = new TaskScheduleEntity();
        saveSchedule(entity, dto);
        appLogService.logInsert(dto, DomainEnum.TASK_DOMAIN_NAME.getValue());
    }

    private void saveSchedule(TaskScheduleEntity entity, TaskScheduleDto dto) {
        if (dto.getCreationDate() == null) {
            dto.setCreationDate(LocalDate.now());
        }
        TaskScheduleFactory.fillEntity(entity, dto);
        TaskScheduleEntity scheduleEntity = repository.save(entity);

        Set<TaskScheduleUserEntity> taskScheduleUserEntitySet = new HashSet<>();
        dto.getUserSet().forEach(userDto -> {
                    UserEntity userEntity = new UserEntity();
                    UserFactory.fillEntity(userEntity, userDto);
                    taskScheduleUserEntitySet.add(
                            new TaskScheduleUserEntity(new TaskScheduleUserId(scheduleEntity, userEntity)));
                }
        );
        taskScheduleUserRepository.deleteByIdTaskScheduleEntityId(scheduleEntity.getId());
        taskScheduleUserRepository.saveAll(taskScheduleUserEntitySet);

        Set<TaskScheduleRoleEntity> taskScheduleRoleEntitySet = new HashSet<>();
        dto.getRoleSet().forEach(roleDto -> {
                    RoleEntity roleEntity = new RoleEntity();
                    RoleFactory.fillEntity(roleEntity, roleDto);
                    taskScheduleRoleEntitySet.add(new TaskScheduleRoleEntity(
                            new TaskScheduleRoleId(scheduleEntity, roleEntity)));
                }
        );
        taskScheduleRoleRepository.deleteByIdTaskScheduleEntityId(scheduleEntity.getId());
        taskScheduleRoleRepository.saveAll(taskScheduleRoleEntitySet);

        List<TaskCheckEntity> taskCheckEntityList = new ArrayList<>();
        dto.getTaskCheckList().forEach(taskCheckDto -> {
            taskCheckDto.setEntityType(TaskEntityType.TEMPLATE.name());
            taskCheckDto.setEntityId(scheduleEntity.getId());
            TaskCheckEntity taskCheckEntity = new TaskCheckEntity();
            TaskCheckFactory.fillEntity(taskCheckEntity, taskCheckDto);
            taskCheckEntityList.add(taskCheckEntity);
        });
        taskCheckRepository.saveAll(taskCheckEntityList);
    }

    @Transactional
    public void updateSchedule(TaskScheduleDto dto, TaskScheduleDto originalDto) throws SystemException {
        TaskScheduleEntity entity = getScheduleEntity(dto.getId());
        saveSchedule(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.TASK_DOMAIN_NAME.getValue());
    }

    public List<TaskCheckDto> getTaskChecklist(Long id) {
        return ConvertEntities.fromEntities(taskCheckRepository.findListByIdAndType(
                id, TaskEntityType.SCHEDULE.name()), TaskCheckFactory::fromEntity);
    }

    private TaskScheduleEntity getScheduleEntity(Long id) throws SystemException {
        return repository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.TASK_SCHEDULE_NOT_EXISTS, id));
    }
}
