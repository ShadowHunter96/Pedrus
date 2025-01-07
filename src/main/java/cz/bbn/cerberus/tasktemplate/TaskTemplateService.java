package cz.bbn.cerberus.tasktemplate;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.role.factory.RoleFactory;
import cz.bbn.cerberus.role.persistance.entity.RoleEntity;
import cz.bbn.cerberus.task.dto.TaskCheckDto;
import cz.bbn.cerberus.task.dto.TaskEntityType;
import cz.bbn.cerberus.task.factory.TaskCheckFactory;
import cz.bbn.cerberus.task.persitance.entity.TaskCheckEntity;
import cz.bbn.cerberus.task.persitance.repository.TaskCheckRepository;
import cz.bbn.cerberus.tasktemplate.dto.TaskTemplateDto;
import cz.bbn.cerberus.tasktemplate.dto.TaskTemplateFilterDto;
import cz.bbn.cerberus.tasktemplate.factory.TaskTemplateFactory;
import cz.bbn.cerberus.tasktemplate.persistance.TaskTemplateDao;
import cz.bbn.cerberus.tasktemplate.persistance.entity.TaskTemplateEntity;
import cz.bbn.cerberus.tasktemplate.persistance.entity.TaskTemplateRoleEntity;
import cz.bbn.cerberus.tasktemplate.persistance.entity.TaskTemplateRoleId;
import cz.bbn.cerberus.tasktemplate.persistance.entity.TaskTemplateUserEntity;
import cz.bbn.cerberus.tasktemplate.persistance.entity.TaskTemplateUserId;
import cz.bbn.cerberus.tasktemplate.persistance.repository.TaskTemplateRepository;
import cz.bbn.cerberus.tasktemplate.persistance.repository.TaskTemplateRoleRepository;
import cz.bbn.cerberus.tasktemplate.persistance.repository.TaskTemplateUserRepository;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Service
public class TaskTemplateService {

    private final TaskTemplateDao taskTemplateDao;
    private final TaskTemplateRepository taskTemplateRepository;
    private final TaskCheckRepository taskCheckRepository;
    private final TaskTemplateUserRepository taskTemplateUserRepository;
    private final TaskTemplateRoleRepository taskTemplateRoleRepository;
    private final AppLogService appLogService;

    public TaskTemplateService(TaskTemplateDao taskTemplateDao, TaskTemplateRepository taskTemplateRepository,
                               TaskCheckRepository taskCheckRepository,
                               TaskTemplateUserRepository taskTemplateUserRepository,
                               TaskTemplateRoleRepository taskTemplateRoleRepository, AppLogService appLogService) {
        this.taskTemplateDao = taskTemplateDao;
        this.taskTemplateRepository = taskTemplateRepository;
        this.taskCheckRepository = taskCheckRepository;
        this.taskTemplateUserRepository = taskTemplateUserRepository;
        this.taskTemplateRoleRepository = taskTemplateRoleRepository;
        this.appLogService = appLogService;
    }

    public Page<TaskTemplateDto> findTaskDtoPage(TaskTemplateFilterDto filter) {
        return taskTemplateDao.findTaskTemplateDtoPage(filter);
    }

    public List<TaskCheckDto> getTaskChecklist(Long id) {
        return ConvertEntities.fromEntities(taskCheckRepository.findListByIdAndType(id, TaskEntityType.TEMPLATE.name()),
                TaskCheckFactory::fromEntity);
    }

    @Transactional
    public void saveTemplate(TaskTemplateDto dto) {
        TaskTemplateEntity entity = new TaskTemplateEntity();
        saveTemplate(entity, dto);
        appLogService.logInsert(dto, DomainEnum.TASK_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void updateTemplate(TaskTemplateDto dto, TaskTemplateDto originalDto) throws SystemException {
        TaskTemplateEntity entity = getTemplateEntity(dto.getId());
        saveTemplate(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.TASK_DOMAIN_NAME.getValue());
    }

    private void saveTemplate(TaskTemplateEntity entity, TaskTemplateDto dto) {
        TaskTemplateFactory.fillEntity(entity, dto);
        TaskTemplateEntity templateEntity = taskTemplateRepository.save(entity);

        Set<TaskTemplateUserEntity> taskTemplateUserEntitySet = new HashSet<>();
        dto.getUserDtoSet().forEach(userDto -> {
                    UserEntity userEntity = new UserEntity();
                    UserFactory.fillEntity(userEntity, userDto);
                    taskTemplateUserEntitySet.add(
                            new TaskTemplateUserEntity(new TaskTemplateUserId(templateEntity, userEntity)));
                }
        );
        taskTemplateUserRepository.deleteByIdTaskTemplateEntityId(templateEntity.getId());
        taskTemplateUserRepository.saveAll(taskTemplateUserEntitySet);

        Set<TaskTemplateRoleEntity> taskTemplateRoleEntitySet = new HashSet<>();
        dto.getRoleDtoSet().forEach(roleDto -> {
                    RoleEntity roleEntity = new RoleEntity();
                    RoleFactory.fillEntity(roleEntity, roleDto);
                    taskTemplateRoleEntitySet.add(new TaskTemplateRoleEntity(
                            new TaskTemplateRoleId(templateEntity, roleEntity)));
                }
        );
        taskTemplateRoleRepository.deleteByIdTaskTemplateEntityId(templateEntity.getId());
        taskTemplateRoleRepository.saveAll(taskTemplateRoleEntitySet);

        List<TaskCheckEntity> taskCheckEntityList = new ArrayList<>();
        dto.getTaskCheckDtoList().forEach(taskCheckDto -> {
            taskCheckDto.setEntityType(TaskEntityType.TEMPLATE.name());
            taskCheckDto.setEntityId(templateEntity.getId());
            TaskCheckEntity taskCheckEntity = new TaskCheckEntity();
            TaskCheckFactory.fillEntity(taskCheckEntity, taskCheckDto);
            taskCheckEntityList.add(taskCheckEntity);
        });
        taskCheckRepository.saveAll(taskCheckEntityList);
    }

    private TaskTemplateEntity getTemplateEntity(Long id) throws SystemException {
        return taskTemplateRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.TASK_TEMPLATE_NOT_EXISTS, id));
    }

    public List<TaskTemplateDto> getAllowedTemplateList() {
        return ConvertEntities.fromEntities(
                taskTemplateRepository.findAllowedTemplateList(
                        SecurityUtils.getCurrentUser().getActiveRoleSet()), TaskTemplateFactory::fromEntity);
    }

    public void deleteById(String id) throws SystemException {
        if (id != null && !id.isEmpty()) {
            TaskTemplateEntity entity = taskTemplateRepository.findById(Long.parseLong(id)).orElseThrow(
                    () -> new SystemException(ErrorCode.TASK_TEMPLATE_NOT_EXISTS, id)
            );
            entity.setDeleted(true);
            taskTemplateRepository.save(entity);
        }
    }
}
