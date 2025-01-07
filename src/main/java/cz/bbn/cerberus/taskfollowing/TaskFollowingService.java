package cz.bbn.cerberus.taskfollowing;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.task.dto.TaskDto;
import cz.bbn.cerberus.task.factory.TaskFactory;
import cz.bbn.cerberus.task.persitance.entity.TaskEntity;
import cz.bbn.cerberus.task.persitance.repository.TaskByUserRepository;
import cz.bbn.cerberus.task.persitance.repository.TaskRepository;
import cz.bbn.cerberus.taskfollowing.dto.TaskFollowingDto;
import cz.bbn.cerberus.taskfollowing.dto.TaskFollowingFilterDto;
import cz.bbn.cerberus.taskfollowing.factory.TaskFollowingFactory;
import cz.bbn.cerberus.taskfollowing.persistance.TaskFollowingDao;
import cz.bbn.cerberus.taskfollowing.persistance.TaskFollowingEntity;
import cz.bbn.cerberus.taskfollowing.persistance.TaskFollowingId;
import cz.bbn.cerberus.taskfollowing.persistance.TaskFollowingRepository;
import cz.bbn.cerberus.user.persistance.UserEntity;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class TaskFollowingService {

    private final TaskFollowingRepository taskFollowingRepository;
    private final TaskFollowingDao taskFollowingDao;
    private final AppLogService appLogService;
    private final TaskByUserRepository taskByUserRepository;
    private final TaskRepository taskRepository;

    public TaskFollowingService(TaskFollowingRepository taskFollowingRepository, TaskFollowingDao taskFollowingDao,
                                AppLogService appLogService, TaskByUserRepository taskByUserRepository,
                                TaskRepository taskRepository) {
        this.taskFollowingRepository = taskFollowingRepository;
        this.taskFollowingDao = taskFollowingDao;
        this.appLogService = appLogService;
        this.taskByUserRepository = taskByUserRepository;
        this.taskRepository = taskRepository;
    }

    public Page<TaskFollowingDto> findTaskFollowingDtoPage(TaskFollowingFilterDto filter) {
        Page<TaskFollowingDto> taskFollowingDtoPage = taskFollowingDao.findTaskFollowingDtoPage(filter);
        Set<Long> userIdSet = taskFollowingDtoPage.stream().collect(Collectors.toSet())
                .stream()
                .map(obj -> obj.getFollowingUserDto().getId())
                .collect(Collectors.toSet());
        Set<TaskEntity> taskEntitySet = taskByUserRepository.findByUserIdSet(userIdSet);
        taskEntitySet.addAll(taskRepository.findByUserIdSet(userIdSet));
        taskFollowingDtoPage.stream().collect(Collectors.toSet()).forEach(taskFollowingDto -> {

            Set<TaskEntity> taskEntitiesSetActual = taskEntitySet.stream().filter(taskEntity ->
                    taskFollowingDto.getFollowingUserDto().getId().equals(taskEntity.getUserEntity().getId())
            ).collect(Collectors.toSet());

            taskEntitySet.forEach(taskEntity -> taskEntity.getTaskUserEntitySet().forEach(taskByUserEntity -> {
                if (taskByUserEntity.getId().getUserEntity().getId()
                        .equals(taskFollowingDto.getFollowingUserDto().getId())) {
                    taskEntitiesSetActual.add(taskEntity);
                }
            }));

            Set<TaskDto> taskDtoSet = ConvertEntities.fromEntities(taskEntitiesSetActual, TaskFactory::fromEntity);
            taskFollowingDto.setTaskDtoList(taskDtoSet);
        });
        return taskFollowingDtoPage;
    }

    public Set<TaskFollowingDto> findAllTaskFollowingDtoSet(Long userId) {
        Set<TaskFollowingEntity> taskFollowingEntitySet = taskFollowingRepository.findAllByIdUserEntityId(userId);
        return ConvertEntities.fromEntities(taskFollowingEntitySet, TaskFollowingFactory::fromEntity);
    }

    @Transactional
    public void deleteTaskFollowing(Long userId, Long followingUserId) throws SystemException {
        TaskFollowingId taskFollowingId = new TaskFollowingId(
                new UserEntity(userId), new UserEntity(followingUserId));
        if (!taskFollowingRepository.existsById(taskFollowingId)) {
            throw new SystemException(ErrorCode.TASK_FOLLOWING_NOT_EXISTS);
        }
        taskFollowingRepository.deleteById(taskFollowingId);
        appLogService.logDelete(String.valueOf(userId).concat(String.valueOf(followingUserId)),
                DomainEnum.TASK_FOLLOWING_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void saveTaskFollowing(Set<TaskFollowingDto> taskFollowingDtoSet) {
        taskFollowingRepository.deleteByIdUserEntityId(SecurityUtils.getCurrentUserId());
        Set<TaskFollowingEntity> taskFollowingEntitySet = new HashSet<>();
        taskFollowingDtoSet.forEach(taskFollowingDto -> {
            TaskFollowingEntity taskFollowingEntity = new TaskFollowingEntity();
            TaskFollowingFactory.fillEntity(taskFollowingEntity, taskFollowingDto);
            taskFollowingEntitySet.add(taskFollowingEntity);
        });
        taskFollowingRepository.saveAll(taskFollowingEntitySet);
    }
}
