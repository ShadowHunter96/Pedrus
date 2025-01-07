package cz.bbn.cerberus.opportunity;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.CustomPermissionProvider;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.custompermission.dto.CustomUserPermissionDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.notification.NotificationService;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.opportunity.dto.OpportunityDtoFilter;
import cz.bbn.cerberus.opportunity.dto.OpportunitySimpleDto;
import cz.bbn.cerberus.opportunity.dto.OpportunityState;
import cz.bbn.cerberus.opportunity.factory.OpportunityFactory;
import cz.bbn.cerberus.opportunity.persistance.dao.OpportunityDao;
import cz.bbn.cerberus.opportunity.persistance.entity.OpportunityEntity;
import cz.bbn.cerberus.opportunity.persistance.repository.OpportunityRepository;
import cz.bbn.cerberus.opportunity.ui.OpportunityDetailView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.subject.ui.SubjectDetailView;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.usermessage.MessageType;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

@Service
public class OpportunityService extends CustomPermissionProvider {

    private final OpportunityDao opportunityDao;
    private final OpportunityRepository opportunityRepository;
    private final ListService listService;
    private final AppLogService appLogService;
    private final CustomPermissionService customPermissionService;
    private final NotificationService notificationService;
    private final AppEnv appEnv;

    public OpportunityService(OpportunityDao opportunityDao, OpportunityRepository opportunityRepository,
                              AppLogService appLogService, ListService listService,
                              CustomPermissionService customPermissionService, NotificationService notificationService,
                              AppEnv appEnv) {
        this.opportunityDao = opportunityDao;
        this.opportunityRepository = opportunityRepository;
        this.appLogService = appLogService;
        this.listService = listService;
        this.customPermissionService = customPermissionService;
        this.notificationService = notificationService;
        this.appEnv = appEnv;
    }

    public Page<OpportunitySimpleDto> findOpportunityDtoPage(OpportunityDtoFilter filter) {
        return opportunityDao.getOpportunityPage(filter);
    }

    public OpportunityDto getOpportunity(String id) throws SystemException {
        OpportunityEntity entity = getEntityById(id);
        return OpportunityFactory.fromEntity(entity);
    }

    public Long getOpportunityCountForDashboard(LocalDate from, LocalDate to) {
        Set<String> objectIdSet = SecurityUtils.getAllowedEntityIdByDomain(
                Permission.OPPORTUNITY_EDIT.name(), DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue());
        return listService.getOpportunityDtoList().stream().filter(opportunityDto -> {
            LocalDate date = opportunityDto.getStartDate();
            OpportunityState state = opportunityDto.getState();
            return date != null && (date.isAfter(from) || date.isEqual(from)) && (date.isBefore(to) || date.isEqual(to))
                    && (state == OpportunityState.NOT_SUBMITED || state == OpportunityState.IN_PROGRESS ||
                    state == OpportunityState.SUBMITTED || state == OpportunityState.WON) &&
                    objectIdSet.contains(opportunityDto.getId());
        }).count();
    }

    public Map<String, Double> getOpportunityMapForDashboard(LocalDate from, LocalDate to) {
        Map<String, Double> map = new HashMap<>();
        Arrays.stream(OpportunityState.values()).forEach(opportunityState -> map.put(opportunityState.name(), 0.0));
        Set<String> objectIdSet = SecurityUtils.getAllowedEntityIdByDomain(
                Permission.OPPORTUNITY_EDIT.name(), DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue());
        listService.getOpportunityDtoList().stream().filter(opportunityDto -> {
            LocalDate date = opportunityDto.getStartDate();
            OpportunityState state = opportunityDto.getState();
            return date != null && (date.isAfter(from) || date.isEqual(from)) && (date.isBefore(to) || date.isEqual(to))
                    && (state == OpportunityState.NOT_SUBMITED || state == OpportunityState.IN_PROGRESS ||
                    state == OpportunityState.SUBMITTED || state == OpportunityState.WON) &&
                    objectIdSet.contains(opportunityDto.getId());
        }).forEach(opportunityDto -> {
            String name = opportunityDto.getState().name();
            Double value = map.get(name) + 1;
            map.put(name, value);

        });
        return map;
    }

    public Map<ItemDto, Double> getOpportunitiesMapByUser(List<UserDto> userDtoList, LocalDate from, LocalDate to) {
        Map<ItemDto, Double> actualMap = new HashMap<>();
        userDtoList.forEach(userDto -> actualMap.put(new ItemDto(userDto), 0D));
        listService.getOpportunityDtoList().stream().forEach(opportunityDto -> {
            LocalDate date = opportunityDto.getStartDate();
            OpportunityState state = opportunityDto.getState();
            if (date != null && (date.isAfter(from) || date.isEqual(from)) && (date.isBefore(to) || date.isEqual(to))
                    && (state == OpportunityState.NOT_SUBMITED || state == OpportunityState.IN_PROGRESS
                    || state == OpportunityState.SUBMITTED || state == OpportunityState.WON)
                    && opportunityDto.getUser() != null
                    && !Boolean.TRUE.equals(opportunityDto.getDeleted())) {
                UserDto userDto = opportunityDto.getUser();
                Double count = actualMap.get(new ItemDto(userDto)) + 1;
                actualMap.put(new ItemDto(userDto), count);
            }
        });

        return AppUtils.removeZeroAndGetMap(actualMap);
    }

    @Transactional
    public void delete(String id) throws SystemException {
        OpportunityEntity entity = getEntityById(id);
        entity.setDeleted(Boolean.TRUE);
        opportunityRepository.save(entity);
        listService.reloadOpportunityDtoList();
    }

    @Transactional
    public void saveOpportunity(OpportunityDto dto) {
        dto.setCreateDate(LocalDateTime.now());
        dto.setId(generateId());
        OpportunityEntity entity = new OpportunityEntity();
        saveOpportunity(entity, dto);
        generateNotificationEntity(dto);
        appLogService.logInsert(dto, DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue());
    }

    private String generateId() {
        OpportunityEntity opportunityEntity = opportunityRepository.findFirstByOrderByIdDesc();
        int sequence = 0;
        if(opportunityEntity != null){
            sequence = AppUtils.getSequenceFromId(opportunityEntity.getId());
        }
        return AppUtils.generateIdSuffix("OP", sequence + 1, appEnv.getContractSuffix());
    }

    @Transactional
    public void updateOwner(Long userId, String id){
        opportunityRepository.updateOwner(userId, id);
    }

    @Transactional
    public void updateOwner(Long ownerId, Long newOwnerId){
        opportunityRepository.updateOwner(ownerId, newOwnerId);
    }

    @Transactional
    public void updateOpportunity(OpportunityDto dto, OpportunityDto originalDto) throws SystemException {
        OpportunityEntity entity = getEntityById(dto.getId());
        saveOpportunity(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue());
    }

    private void saveOpportunity(OpportunityEntity entity, OpportunityDto dto) {
        OpportunityFactory.fillEntity(entity, dto);
        opportunityRepository.save(entity);

        CustomUserPermissionDto customUserPermissionDto = new CustomUserPermissionDto(
                DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue(), CustomPermissionService.ALL_PERMISSION,
                SecurityUtils.getCurrentUserId(), dto.getId(), true
        );
        customPermissionService.saveSinglePermission(customUserPermissionDto);
        appLogService.logInsert(customUserPermissionDto, CustomPermissionService.CUSTOM_PERMISSION_OBJECT_NAME);
        appLogService.logInsert(dto, DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue());
        listService.reloadOpportunityDtoList();
    }

    private OpportunityEntity getEntityById(String id) throws SystemException {
        return opportunityRepository.findById(id)
                .orElseThrow(() -> new SystemException(ErrorCode.OPPORTUNITY_NOT_EXITS, id));
    }

    @Override
    protected List<String> findAllId() {
        return opportunityRepository.findAllId();
    }

    @Override
    public Set<DomainEnum> getDomainSet() {
        return EnumSet.of(DomainEnum.OPPORTUNITY_DOMAIN_NAME);
    }

    @Override
    protected boolean showInCustomPermissions() {
        return true;
    }

    @Override
    public Map<String, Long> getOwnerMap() {
        Map<String, Long> ownerMap = new HashMap<>();
        for (OpportunityDto dto : listService.getOpportunityDtoList()) {
            if (dto.getUser() != null) {
                ownerMap.put(dto.getId(), dto.getUser().getId());
            }
        }
        return ownerMap;
    }

    private void generateNotificationEntity(OpportunityDto opportunityDto) {
        SubjectDto subjectDto = opportunityDto.getSubject();
        UserDto userDto = subjectDto.getUserDto();
        String language = Optional.ofNullable(userDto.getPreferredLanguage()).orElse(Transl.DEFAULT_LANG);

        if (!userDto.getId().equals(SecurityUtils.getCurrentUserId())) {
            String message = Transl.getByLang("User", language).concat(" ")
                    .concat(SecurityUtils.getCurrentUserName()).concat(" ")
                    .concat(Transl.getByLang("created a new opportunity", language).concat(" ")
                            .concat(AppUtils.generateUrl(appEnv.getProjectUrl(), OpportunityDetailView.ROUTE,
                                    opportunityDto.getId().replace("/", "&ndash"),
                                    opportunityDto.getName())).concat(" ")
                            .concat(Transl.getByLang("on subject", language)).concat(" ")
                            .concat(AppUtils.generateUrl(appEnv.getProjectUrl(), SubjectDetailView.ROUTE,
                                    subjectDto.getId(), subjectDto.getName())));
            notificationService.saveEmailLowNotification(MessageType.NEW_OPPORTUNITY.name(), message, userDto.getId());
        }
    }
}
