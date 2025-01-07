package cz.bbn.cerberus.virtualserver;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.notification.NotificationService;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.virtualserver.dto.HddDto;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerAction;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerDto;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerFilterDto;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerStatus;
import cz.bbn.cerberus.virtualserver.factory.HddFactory;
import cz.bbn.cerberus.virtualserver.factory.VirtualServerFactory;
import cz.bbn.cerberus.virtualserver.persistance.HddEntity;
import cz.bbn.cerberus.virtualserver.persistance.HddRepository;
import cz.bbn.cerberus.virtualserver.persistance.VirtualServerDao;
import cz.bbn.cerberus.virtualserver.persistance.VirtualServerEntity;
import cz.bbn.cerberus.virtualserver.persistance.VirtualServerRepository;
import cz.bbn.cerberus.virtualserver.ui.VirtualServerView;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.Map;
import java.util.Objects;

@Service
public class VirtualServerService {

    public static final String OBJECT_NAME = "Virtual server";

    private final VirtualServerDao virtualServerDao;
    private final VirtualServerRepository virtualServerRepository;
    private final HddRepository hddRepository;
    private final AppLogService appLogService;
    private final NotificationService notificationService;
    private final UserService userService;
    private final AppEnv appEnv;

    public VirtualServerService(VirtualServerDao virtualServerDao, VirtualServerRepository virtualServerRepository,
                                HddRepository hddRepository, AppLogService appLogService,
                                NotificationService notificationService, UserService userService, AppEnv appEnv) {
        this.virtualServerDao = virtualServerDao;
        this.virtualServerRepository = virtualServerRepository;
        this.hddRepository = hddRepository;
        this.appLogService = appLogService;
        this.notificationService = notificationService;
        this.userService = userService;
        this.appEnv = appEnv;
    }

    public Page<VirtualServerDto> findVirtualServerDtoPage(VirtualServerFilterDto virtualServerFilterDto) {
        return virtualServerDao.findVirtualServerPage(virtualServerFilterDto);
    }

    @Transactional
    public void deleteVirtualServer(Long id) throws SystemException {
        VirtualServerEntity entity = getVirtualServerEntity(id);
        entity.setStatus(VirtualServerStatus.DELETED.name());
        entity.setDeleted(true);
        virtualServerRepository.save(entity);
        saveEmailNotification(VirtualServerFactory.fromEntity(entity),
                SecurityUtils.getCurrentUserDto(), VirtualServerAction.DELETE);
        appLogService.logDelete(String.valueOf(id), DomainEnum.INFRASTRUCTURE.getValue());
    }

    private VirtualServerEntity getVirtualServerEntity(Long id) throws SystemException {
        return virtualServerRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.VIRTUAL_SERVER_NOT_EXISTS, id));
    }

    @Transactional
    public void updateVirtualServer(VirtualServerDto dto, VirtualServerAction action) throws SystemException {
        VirtualServerEntity entity = getVirtualServerEntity(dto.getId());
        VirtualServerDto originalDto = VirtualServerFactory.fromEntity(entity);
        VirtualServerFactory.fillEntity(entity, dto);
        List<HddEntity> hddEntityList = new ArrayList<>();
        for (HddDto hddDto : dto.getHddDtoList()) {
            HddEntity hddEntity = new HddEntity();
            HddFactory.fillEntity(hddEntity, hddDto);
            hddEntity.setVirtualServerId(dto.getId());
            hddEntityList.add(hddEntity);
        }
        hddRepository.saveAll(hddEntityList);
        virtualServerRepository.save(entity);
        saveEmailNotification(dto, SecurityUtils.getCurrentUserDto(), action);
        appLogService.logUpdate(dto, originalDto, OBJECT_NAME);
    }

    @Transactional
    public void createVirtualServer(VirtualServerDto dto) throws SystemException {
        if (dto.getId() != null && virtualServerRepository.existsById(dto.getId())) {
            throw new SystemException(ErrorCode.VIRTUAL_SERVER_ALREADY_EXISTS, dto.getId());
        }
        VirtualServerEntity entity = new VirtualServerEntity();
        VirtualServerFactory.fillEntity(entity, dto);
        entity.setHddEntityList(new ArrayList<>());
        entity.setDeleted(false);
        entity.setRequestDate(LocalDateTime.now());
        VirtualServerEntity savedEntity = virtualServerRepository.save(entity);
        List<HddEntity> hddEntityList = new ArrayList<>();
        for (HddDto hddDto : dto.getHddDtoList()) {
            HddEntity hddEntity = new HddEntity();
            HddFactory.fillEntity(hddEntity, hddDto);
            hddEntity.setVirtualServerId(savedEntity.getId());
            hddEntityList.add(hddEntity);
        }
        hddRepository.saveAll(hddEntityList);
        saveEmailNotification(dto, SecurityUtils.getCurrentUserDto(), VirtualServerAction.REQUEST);
        appLogService.logInsert(dto, OBJECT_NAME);
        updateVsWithoutId();
    }

    private void updateVsWithoutId() {
        Set<VirtualServerEntity> vsSet = virtualServerRepository.getWithoutId();
        for (VirtualServerEntity vs : vsSet) {
            vs.setStringId(AppUtils.generateId("VS", vs.getId().intValue()));
        }
    }

    private void saveEmailNotification(VirtualServerDto dto, UserDto byUser, VirtualServerAction action) {
        if (action != null) {
            String title = action.getNotificationTitle();
            Map<Long, String> userIdMap = new HashMap<>();

            if (Objects.equals(byUser.getId(), dto.getOwner().getId())) {
                Set<Long> userIdSet = userService.findUserIdListByInfRole();
                userIdSet.forEach(aLong -> userIdMap.put(aLong, getVirtualServerLink(dto)));
            } else {
                userIdMap.put(dto.getOwner().getId(), getVirtualServerLink(dto));
            }

            for (Map.Entry<Long, String> entry : userIdMap.entrySet()) {
                String message = getEmailMessage(dto, entry.getValue(), action);
                notificationService.saveEmailHighNotification(title, message, entry.getKey());
            }
        }
    }

    private String getEmailMessage(VirtualServerDto dto, String link, VirtualServerAction action) {
        String emailMessage = "<br />".concat(Transl.get("Virtual server")).concat(": ").concat("<b>")
                .concat(dto.getName())
                .concat("</b><br /><br />")
                .concat(Transl.get("Action")).concat(": ").concat(Transl.get(action.getAction()))
                .concat("<br /><br />")
                .concat(Transl.get("Date of action execution")).concat(": ")
                .concat(AppUtils.formatDateTime(LocalDateTime.now(), true))
                .concat("<br /><br />");
        if (dto.getIp() != null && !dto.getIp().isEmpty()) {
            emailMessage = emailMessage.concat(Transl.get("IP")).concat(": ").concat(dto.getIp())
                    .concat("<br /><br />");
        }
        return emailMessage.concat(Transl.get("Subnet")).concat(": ")
                .concat(dto.getSubnet().getValue())
                .concat("<br /><br />")
                .concat(link);
    }

    private String getVirtualServerLink(VirtualServerDto dto) {
        return AppUtils.generateUrl(appEnv.getProjectUrl(), VirtualServerView.ROUTE,
                "&id=" + dto.getId(), Transl.get("Click to link"));
    }
}
