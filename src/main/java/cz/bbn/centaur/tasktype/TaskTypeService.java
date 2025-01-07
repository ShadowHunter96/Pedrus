package cz.bbn.cerberus.tasktype;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.role.factory.RoleFactory;
import cz.bbn.cerberus.role.persistance.entity.RoleEntity;
import cz.bbn.cerberus.tasktype.dto.TaskTypeDto;
import cz.bbn.cerberus.tasktype.factory.TaskTypeFactory;
import cz.bbn.cerberus.tasktype.persistance.TaskTypeDao;
import cz.bbn.cerberus.tasktype.persistance.TaskTypeEntity;
import cz.bbn.cerberus.tasktype.persistance.TaskTypeRepository;
import cz.bbn.cerberus.tasktype.persistance.TaskTypeRoleEntity;
import cz.bbn.cerberus.tasktype.persistance.TaskTypeRoleId;
import cz.bbn.cerberus.tasktype.persistance.TaskTypeRoleRepository;
import org.apache.commons.lang3.SerializationUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Service
public class TaskTypeService {

    private final TaskTypeRepository taskTypeRepository;
    private final TaskTypeRoleRepository taskTypeRoleRepository;
    private final AppLogService appLogService;
    private final TaskTypeDao taskTypeDao;

    public TaskTypeService(TaskTypeRepository taskTypeRepository, TaskTypeRoleRepository taskTypeRoleRepository,
                           AppLogService appLogService, TaskTypeDao taskTypeDao) {
        this.taskTypeRepository = taskTypeRepository;
        this.taskTypeRoleRepository = taskTypeRoleRepository;
        this.appLogService = appLogService;
        this.taskTypeDao = taskTypeDao;
    }

    @Transactional
    public void updateTaskType(TaskTypeDto dto, TaskTypeDto originalDto) throws SystemException {
        TaskTypeEntity entity = getTaskTypeEntity(dto.getId());
        this.saveTaskType(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.ADMINISTRATION_DOMAIN_NAME.getValue());
    }

    @Transactional
    public Long saveTaskType(TaskTypeDto dto) throws SystemException {
        if (dto.getId() != null && taskTypeRepository.existsById(dto.getId())) {
            throw new SystemException(ErrorCode.ID_ALREADY_EXISTS, dto.getId());
        }
        TaskTypeEntity entity = new TaskTypeEntity();
        Long toReturn = saveTaskType(entity, dto);
        appLogService.logInsert(dto, DomainEnum.ADMINISTRATION_DOMAIN_NAME.getValue());
        return toReturn;
    }

    public TaskTypeDto getTaskType(Long id) throws SystemException {
        TaskTypeEntity entity = getTaskTypeEntity(id);
        return TaskTypeFactory.fromEntity(entity);
    }

    private Long saveTaskType(TaskTypeEntity entity, TaskTypeDto dto) {
        TaskTypeFactory.fillEntity(entity, dto);
        TaskTypeEntity taskTypeEntity = taskTypeRepository.save(entity);
        Set<TaskTypeRoleEntity> taskTypeRoleEntitySet = new HashSet<>();
        dto.getRoleSet().forEach(roleDto -> {
                    RoleEntity roleEntity = new RoleEntity();
                    RoleFactory.fillEntity(roleEntity, roleDto);
                    taskTypeRoleEntitySet.add(new TaskTypeRoleEntity(new TaskTypeRoleId(taskTypeEntity, roleEntity)));
                }
        );
        taskTypeRoleRepository.deleteByIdTaskTypeEntityId(taskTypeEntity.getId());
        taskTypeRoleRepository.saveAll(taskTypeRoleEntitySet);
        return taskTypeEntity.getId();
    }

    private TaskTypeEntity getTaskTypeEntity(Long id) throws SystemException {
        return taskTypeRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.TASK_TYPE_NOT_EXISTS, id));
    }

    public void deleteTaskType(Long id) throws SystemException {
        if (!taskTypeExists(id)) {
            throw new SystemException(ErrorCode.TASK_TYPE_NOT_EXISTS, id);
        }
        TaskTypeEntity entity = getTaskTypeEntity(id);
        TaskTypeDto originalDto = TaskTypeFactory.fromEntity(entity);
        TaskTypeDto newDto = SerializationUtils.clone(originalDto);
        originalDto.setArchived(true);
        entity.setArchived(true);
        taskTypeRepository.save(entity);
        appLogService.logUpdate(newDto, originalDto, DomainEnum.ADMINISTRATION_DOMAIN_NAME.getValue());
    }

    public Page<TaskTypeDto> findTaskTypeDtoPage(int page, int size, List<Sort.Order> sortList) {
        return taskTypeDao.findTaskTypeDtoPage(page, size, sortList);
    }

    public boolean taskTypeExists(Long id) {
        return taskTypeRepository.existsById(id);
    }

    public List<TaskTypeDto> getAllowedTaskTypeList(ObjectType objectType) {
        Set<String> roleStrSet = SecurityUtils.getCurrentUser().getRoleSet();
        List<TaskTypeDto> taskTypeDtoList = ConvertEntities.fromEntities(
                taskTypeRepository.findAllNonArchived(), TaskTypeFactory::fromEntity);
        List<TaskTypeDto> toReturnList = new ArrayList<>();
        for (TaskTypeDto dto : taskTypeDtoList) {
            Set<String> dtoRoleStrSet = new HashSet<>();
            for (RoleDto roleDto : dto.getRoleSet()) {
                dtoRoleStrSet.add(roleDto.getId());
            }
            if ((objectType == null || dto.getObjectTypeSet().contains(objectType))
                    && !Collections.disjoint(roleStrSet, dtoRoleStrSet)) {
                toReturnList.add(dto);
            }
        }
        return toReturnList;
    }

    public List<TaskTypeDto> getAllAllowed() {
        return ConvertEntities.fromEntities(taskTypeRepository.findAllNonArchived(), TaskTypeFactory::fromEntity);
    }
}
