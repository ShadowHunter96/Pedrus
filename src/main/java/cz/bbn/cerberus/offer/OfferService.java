package cz.bbn.cerberus.offer;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.CustomPermissionProvider;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.notification.NotificationService;
import cz.bbn.cerberus.offer.dto.OfferDto;
import cz.bbn.cerberus.offer.dto.OfferFilterDto;
import cz.bbn.cerberus.offer.dto.OfferState;
import cz.bbn.cerberus.offer.factory.OfferFactory;
import cz.bbn.cerberus.offer.repository.OfferDao;
import cz.bbn.cerberus.offer.repository.entity.OfferEntity;
import cz.bbn.cerberus.offer.repository.repository.OfferRepository;
import cz.bbn.cerberus.offer.ui.OfferDetailView;
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
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

@Service
public class OfferService extends CustomPermissionProvider {

    private final OfferDao offerDao;
    private final OfferRepository offerRepository;
    private final AppLogService appLogService;
    private final ListService listService;
    private final NotificationService notificationService;
    private final AppEnv appEnv;

    public OfferService(OfferDao offerDao, OfferRepository offerRepository, AppLogService appLogService,
                        ListService listService, NotificationService notificationService, AppEnv appEnv) {
        this.offerDao = offerDao;
        this.offerRepository = offerRepository;
        this.appLogService = appLogService;
        this.listService = listService;
        this.notificationService = notificationService;
        this.appEnv = appEnv;
    }

    public Page<OfferDto> findOfferDtoPage(OfferFilterDto filter) {
        return offerDao.findOfferPage(filter);
    }

    public OfferDto getOfferDto(String id) throws SystemException {
        OfferEntity entity = getEntityById(id);
        return OfferFactory.fromEntity(entity);
    }

    public Long getOfferCountForDashboard(LocalDate from, LocalDate to) {
        Set<String> objectIdSet = SecurityUtils
                .getAllowedEntityIdByDomain(
                        Permission.OPPORTUNITY_EDIT.name(), DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue());
        return listService.getOfferDtoList().stream().filter(offerDto -> {
            LocalDate date = offerDto.getValidityDate();
            return date != null && (date.isAfter(from) || date.isEqual(from))
                    && (date.isBefore(to) || date.isEqual(to))
                    && objectIdSet.contains(offerDto.getOpportunityDto().getId());
        }).count();
    }

    public Map<String, Double> getOfferMapForDashboard(LocalDate from, LocalDate to) {
        Map<String, Double> map = new HashMap<>();
        Arrays.stream(OfferState.values()).forEach(offerState -> map.put(offerState.name(), 0.0));
        Set<String> objectIdSet = SecurityUtils
                .getAllowedEntityIdByDomain(Permission.OFFER_EDIT.name(), DomainEnum.OFFER_DOMAIN_NAME.getValue());
        listService.getOfferDtoList().stream().filter(offerDto -> {
            LocalDate date = offerDto.getValidityDate();
            return date != null && (date.isAfter(from) || date.isEqual(from))
                    && (date.isBefore(to) || date.isEqual(to)) && objectIdSet.contains(offerDto.getId());
        }).forEach(offerDto -> {
            String name = offerDto.getState().name();
            Double value = map.get(name) + 1;
            map.put(name, value);

        });
        return map;
    }

    public Map<ItemDto, Double> getOffersMapByUser(List<UserDto> userDtoList, LocalDate from, LocalDate to) {
        Map<ItemDto, Double> actualMap = AppUtils.getUserMapWithDouble(userDtoList);
        listService.getOfferDtoList().stream().forEach(offerDto -> {
            LocalDate date = offerDto.getValidityDate();
            if (!Boolean.TRUE.equals(offerDto.getDeleted())
                    && date != null && (date.isAfter(from) || date.isEqual(from)) && (date.isBefore(to)
                    || date.isEqual(to)) && offerDto.getProcessedByUserDto() != null) {
                UserDto userDto = offerDto.getProcessedByUserDto();
                Double count = actualMap.get(new ItemDto(userDto)) + 1;
                actualMap.put(new ItemDto(userDto), count);
            }
        });
        return AppUtils.removeZeroAndGetMap(actualMap);
    }

    @Transactional
    public void saveOffer(OfferDto dto) throws SystemException {
        OfferEntity entity = new OfferEntity();
        int sequence = 0;
        OfferEntity offerEntity = offerRepository.findFirstByOrderByIdDesc();
        if(offerEntity != null){
            sequence = AppUtils.getSequenceFromId(offerEntity.getId());
        }
        dto.setId(AppUtils.generateId("OF", sequence + 1));
        saveOffer(entity, dto);
        generateNotificationEntity(dto);
        appLogService.logInsert(dto, DomainEnum.OFFER_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void updateOffer(OfferDto dto, OfferDto originalDto) throws SystemException {
        OfferEntity entity = getEntityById(dto.getId());
        saveOffer(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.OFFER_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void delete(String id) throws SystemException {
        OfferEntity entity = getEntityById(id);
        entity.setDeleted(Boolean.TRUE);
        listService.reloadOfferDtoList();
    }

    @Transactional
    public void updateOwner(Long userId, String id){
        offerRepository.updateOwner(userId, id);
    }

    @Transactional
    public void updateOwner(Long ownerId, Long newOwnerId){
        offerRepository.updateOwner(ownerId, newOwnerId);
    }

    private void saveOffer(OfferEntity entity, OfferDto dto) {
        OfferFactory.fillEntity(entity, dto);
        offerRepository.save(entity);
        listService.reloadOfferDtoList();
    }

    private OfferEntity getEntityById(String id) throws SystemException {
        return offerRepository.findById(id)
                .orElseThrow(() -> new SystemException(ErrorCode.OFFER_NOT_EXITS, id));
    }

    @Override
    protected List<String> findAllId() {
        return offerRepository.findAllId();
    }

    @Override
    protected Set<DomainEnum> getDomainSet() {
        return EnumSet.of(DomainEnum.OFFER_DOMAIN_NAME);
    }

    @Override
    protected boolean showInCustomPermissions() {
        return true;
    }

    @Override
    protected Map<String, Long> getOwnerMap() {
        Map<String, Long> ownerMap = new HashMap<>();
        for (OfferDto dto : listService.getOfferDtoList()) {
            if (dto.getProcessedByUserDto() != null) {
                ownerMap.put(dto.getId(), dto.getProcessedByUserDto().getId());
            }
        }
        return ownerMap;
    }

    private void generateNotificationEntity(OfferDto offerDto) {
        SubjectDto subjectDto = offerDto.getSubjectDto();
        UserDto userDto = subjectDto.getUserDto();
        String language = Optional.ofNullable(userDto.getPreferredLanguage()).orElse(Transl.DEFAULT_LANG);

        if (!userDto.getId().equals(SecurityUtils.getCurrentUserId())) {
            String message = Transl.getByLang("User", language).concat(" ")
                    .concat(SecurityUtils.getCurrentUserName()).concat(" ")
                    .concat(Transl.getByLang("created a new offer", language).concat(" ")
                            .concat(AppUtils.generateUrl(appEnv.getProjectUrl(), OfferDetailView.ROUTE,
                                    offerDto.getId().replace("/", "&ndash"), offerDto.getName())).concat(" ")
                            .concat(Transl.getByLang("on subject", language)).concat(" ")
                            .concat(AppUtils.generateUrl(appEnv.getProjectUrl(), SubjectDetailView.ROUTE,
                                    subjectDto.getId(), subjectDto.getName())));
            notificationService.saveEmailLowNotification(MessageType.NEW_OFFER.name(), message, userDto.getId());
        }
    }

}
