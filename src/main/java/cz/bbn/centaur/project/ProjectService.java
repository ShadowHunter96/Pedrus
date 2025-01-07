package cz.bbn.cerberus.project;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.domain.ItemEntity;
import cz.bbn.cerberus.commons.component.ui.factory.ItemFactory;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.CustomPermissionProvider;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.custompermission.dto.CustomUserPermissionDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.notification.NotificationService;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.project.dto.ProjectFilterDto;
import cz.bbn.cerberus.project.dto.ProjectSimpleDto;
import cz.bbn.cerberus.project.factory.ProjectFactory;
import cz.bbn.cerberus.project.persistance.ProjectDao;
import cz.bbn.cerberus.project.persistance.ProjectSimpleDao;
import cz.bbn.cerberus.project.persistance.entity.ProjectEntity;
import cz.bbn.cerberus.project.persistance.repository.ProjectRepository;
import cz.bbn.cerberus.project.persistance.repository.ProjectSimpleRepository;
import cz.bbn.cerberus.project.ui.ProjectDetailView;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.subject.ui.SubjectDetailView;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.usermessage.MessageType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

@Service
public class ProjectService extends CustomPermissionProvider {

    private final ProjectDao projectDao;
    private final ProjectSimpleDao projectSimpleDao;
    private final ProjectRepository projectRepository;
    private final ProjectSimpleRepository projectSimpleRepository;
    private final AppLogService appLogService;
    private final CustomPermissionService customPermissionService;
    private final ListService listService;
    private final NotificationService notificationService;
    private final AppEnv appEnv;

    public ProjectService(ProjectDao projectDao, ProjectSimpleDao projectSimpleDao,
                          ProjectRepository projectRepository, ProjectSimpleRepository projectSimpleRepository,
                          AppLogService appLogService, CustomPermissionService customPermissionService,
                          ListService listService, NotificationService notificationService, AppEnv appEnv) {
        this.projectDao = projectDao;
        this.projectSimpleDao = projectSimpleDao;
        this.projectRepository = projectRepository;
        this.projectSimpleRepository = projectSimpleRepository;
        this.appLogService = appLogService;
        this.customPermissionService = customPermissionService;
        this.listService = listService;
        this.notificationService = notificationService;
        this.appEnv = appEnv;
    }

    public Page<ProjectSimpleDto> findProjectDtoPage(ProjectFilterDto filter) {
        return projectSimpleDao.findProjectPage(filter);
    }

    public List<ProjectDto> findProjectDtoAllowedList() {
        return projectDao.findProjectList();
    }

    public List<ItemDto> findItemDtoList() {
        Set<String> projectIdSet = SecurityUtils.getCustomReadPermission(DomainEnum.PROJECT_DOMAIN_NAME.getValue());
        List<ItemEntity> entityList = projectSimpleRepository.findAllAllowedItemList(projectIdSet);
        return ConvertEntities
                .fromEntities(entityList, ItemFactory::fromEntity);
    }

    public ProjectDto getProject(String id) throws SystemException {
        ProjectEntity entity = getProjectEntity(id);
        return ProjectFactory.fromEntity(entity);
    }

    public Long getProjectCountForDashboard() {
        Set<String> objectIdSet = SecurityUtils
                .getAllowedEntityIdByDomain(Permission.PROJECT_EDIT.name(), DomainEnum.PROJECT_DOMAIN_NAME.getValue());
        return listService.getProjectDtoList().stream().filter(
                projectDto -> projectDto.getProjectState() != null
                        && projectDto.getProjectState() == ProjectState.REALIZE
                        && projectDto.getUserDto() != null
                        && !Boolean.TRUE.equals(projectDto.isDeleted())
                        && objectIdSet.contains(projectDto.getId())).count();
    }

    public Map<ItemDto, Double> getProjectsMapByUser(List<UserDto> userDtoList) {
        Map<ItemDto, Double> actualMap = AppUtils.getUserMapWithDouble(userDtoList);
        listService.getProjectDtoList().stream().forEach(projectDto -> {
            if (projectDto.getProjectState() != null && projectDto.getProjectState() == ProjectState.REALIZE
                    && projectDto.getUserDto() != null && !Boolean.TRUE.equals(projectDto.isDeleted())) {
                UserDto userDto = projectDto.getUserDto();
                Double count = actualMap.get(new ItemDto(userDto)) + 1;
                actualMap.put(new ItemDto(userDto), count);
            }
        });
        return AppUtils.removeZeroAndGetMap(actualMap);
    }

    @Transactional
    public void saveProject(ProjectDto dto) throws SystemException {
        dto.setId(AppUtils.generateId("PR", getLastSequence()));

        ProjectEntity entity = new ProjectEntity();
        saveProject(entity, dto);

        CustomUserPermissionDto customUserPermissionDto = new CustomUserPermissionDto(
                DomainEnum.PROJECT_DOMAIN_NAME.getValue(), CustomPermissionService.ALL_PERMISSION,
                dto.getUserDto().getId(), dto.getId(), true
        );

        customPermissionService.saveSinglePermission(customUserPermissionDto);
        generateNotificationEntity(dto);
        appLogService.logInsert(customUserPermissionDto, CustomPermissionService.CUSTOM_PERMISSION_OBJECT_NAME);
        listService.reloadProjectList();
        appLogService.logInsert(dto, DomainEnum.PROJECT_DOMAIN_NAME.getValue());
        customPermissionService.loadPermissionSetByCurrentUser();
    }

    @Transactional
    public void updateProject(ProjectDto dto, ProjectDto originalDto) throws SystemException {
        ProjectEntity entity = getProjectEntity(dto.getId());
        saveProject(entity, dto);

        listService.reloadProjectList();
        appLogService.logUpdate(dto, originalDto, DomainEnum.PROJECT_DOMAIN_NAME.getValue());
        customPermissionService.loadPermissionSetByCurrentUser();
    }

    @Transactional
    public void deleteProject(String id) throws SystemException {
        ProjectEntity entity = getProjectEntity(id);
        entity.setDeleted(true);
        projectRepository.save(entity);
        listService.reloadProjectList();
        appLogService.logDelete(id, DomainEnum.PROJECT_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void updateOwner(Long userId, String id){
        projectRepository.updateOwner(userId, id);
    }

    @Transactional
    public void updateOwner(Long ownerId, Long newOwnerId){
        projectRepository.updateOwner(ownerId, newOwnerId);
    }

    @Override
    public List<String> findAllId() {
        return projectSimpleRepository.findAllId();
    }

    @Override
    public Set<DomainEnum> getDomainSet() {
        return EnumSet.of(DomainEnum.PROJECT_DOMAIN_NAME);
    }

    @Override
    protected boolean showInCustomPermissions() {
        return true;
    }

    @Override
    public Map<String, Long> getOwnerMap() {
        Map<String, Long> ownerMap = new HashMap<>();
        for (ProjectDto dto : listService.getProjectDtoList()) {
            if (dto.getUserDto() != null) {
                ownerMap.put(dto.getId(), dto.getUserDto().getId());
            }
        }
        return ownerMap;
    }

    private void saveProject(ProjectEntity entity, ProjectDto dto) {
        ProjectFactory.fillEntity(entity, dto);
        projectRepository.save(entity);
    }

    private ProjectEntity getProjectEntity(String id) throws SystemException {
        return projectRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.PROJECT_NOT_EXISTS, id));
    }

    private void generateNotificationEntity(ProjectDto projectDto) {
        SubjectDto subjectDto = projectDto.getSubject();
        UserDto userDto = subjectDto.getUserDto();
        String language = Optional.ofNullable(userDto.getPreferredLanguage()).orElse(Transl.DEFAULT_LANG);

        if (!userDto.getId().equals(SecurityUtils.getCurrentUserId())) {
            String message = Transl.getByLang("User", language).concat(" ").concat(SecurityUtils.getCurrentUserName()).concat(" ")
                    .concat(Transl.getByLang("created a new project", language).concat(" ")
                            .concat(AppUtils.generateUrl(appEnv.getProjectUrl(), ProjectDetailView.ROUTE, projectDto.getId(), projectDto.getName())).concat(" ")
                            .concat(Transl.getByLang("on subject", language)).concat(" ")
                            .concat(AppUtils.generateUrl(appEnv.getProjectUrl(), SubjectDetailView.ROUTE, subjectDto.getId(), subjectDto.getName())));
            notificationService.saveEmailLowNotification(MessageType.NEW_PROJECT.name(), message, userDto.getId());
        }
    }

    private int getLastSequence() {
        String lastId = projectRepository.findLastId(PageRequest.of(0, 1));
        if (lastId != null && !lastId.isEmpty()) {
            int lastSequence = AppUtils.getSequenceFromId(lastId);
            if (lastSequence > 0) {
                return lastSequence + 1;
            }
        }
        return 1;
    }

}
