package cz.bbn.cerberus.dssetting;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.CustomPermissionProvider;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.custompermission.dto.CustomUserPermissionDto;
import cz.bbn.cerberus.dssetting.dto.DsSettingDto;
import cz.bbn.cerberus.dssetting.dto.DsSettingFilterDto;
import cz.bbn.cerberus.dssetting.dto.DsSettingSimpleDto;
import cz.bbn.cerberus.dssetting.factory.DsSettingFactory;
import cz.bbn.cerberus.dssetting.persistance.DsSettingDao;
import cz.bbn.cerberus.dssetting.persistance.DsSettingEntity;
import cz.bbn.cerberus.dssetting.persistance.DsSettingRepository;
import cz.bbn.cerberus.dssetting.persistance.DsSettingSimpleRepository;
import cz.bbn.cerberus.listconfiguration.ListService;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.io.IOException;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Service
public class DsSettingService extends CustomPermissionProvider {

    private final DsSettingDao dsSettingDao;
    private final DsSettingSimpleRepository dsSettingSimpleRepository;
    private final DsSettingRepository dsSettingRepository;
    private final AppLogService appLogService;
    private final CustomPermissionService customPermissionService;
    private final ListService listService;

    public DsSettingService(DsSettingDao dsSettingDao, DsSettingSimpleRepository dsSettingSimpleRepository,
                            DsSettingRepository dsSettingRepository, AppLogService appLogService,
                            CustomPermissionService customPermissionService, ListService listService) {
        this.dsSettingDao = dsSettingDao;
        this.dsSettingSimpleRepository = dsSettingSimpleRepository;
        this.dsSettingRepository = dsSettingRepository;
        this.appLogService = appLogService;
        this.customPermissionService = customPermissionService;
        this.listService = listService;
    }

    public Page<DsSettingSimpleDto> findDsSettingDtoPage(DsSettingFilterDto filter) {
        return dsSettingDao.findDsSettingDtoPage(filter);
    }

    public DsSettingDto getDsSettingDto(String id) throws SystemException {
        DsSettingEntity entity = getDsEntity(id);
        return DsSettingFactory.fromEntity(entity);
    }

    public boolean dsSettingExists(String id) {
        return dsSettingSimpleRepository.existsById(id);
    }

    @Transactional
    public void saveDsSetting(DsSettingDto dto) throws SystemException, IOException {
        if (dsSettingExists(dto.getId())) {
            throw new SystemException(ErrorCode.ID_ALREADY_EXISTS, dto.getId());
        }
        DsSettingEntity entity = new DsSettingEntity();
        saveDsSetting(entity, dto);

        CustomUserPermissionDto customUserPermissionDto = new CustomUserPermissionDto(
                DomainEnum.DS_SETTING_DOMAIN_NAME.getValue(), CustomPermissionService.ALL_PERMISSION,
                dto.getUserDto().getId(), dto.getId(), true
        );

        customPermissionService.saveSinglePermission(customUserPermissionDto);
        appLogService.logInsert(dto, DomainEnum.DS_SETTING_DOMAIN_NAME.getValue());
        customPermissionService.loadPermissionSetByCurrentUser();
    }

    @Transactional
    public void updateDsSetting(DsSettingDto dto, DsSettingDto originalDto) throws SystemException, IOException {
        DsSettingEntity entity = getDsEntity(dto.getId());
        this.saveDsSetting(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.DS_SETTING_DOMAIN_NAME.getValue());
        customPermissionService.loadPermissionSetByCurrentUser();
    }

    @Transactional
    public void deleteDsSetting(String id) throws SystemException {
        if (!dsSettingExists(id)) {
            throw new SystemException(ErrorCode.DS_SETTINGS_NOT_EXISTS, id);
        }
        DsSettingEntity entity = getDsEntity(id);
        entity.setDeleted(!entity.getDeleted());
        dsSettingRepository.save(entity);
        appLogService.logDelete(id, DomainEnum.DS_SETTING_DOMAIN_NAME.getValue());
    }


    private void saveDsSetting(DsSettingEntity entity, DsSettingDto dto) throws IOException {
        DsSettingFactory.fillEntity(entity, dto);
        dsSettingRepository.save(entity);
    }

    private DsSettingEntity getDsEntity(String id) throws SystemException {
        return dsSettingRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.DS_SETTINGS_NOT_EXISTS, id));
    }

    @Override
    protected List<String> findAllId() {
        return dsSettingRepository.findAllId();
    }

    @Override
    public Set<DomainEnum> getDomainSet() {
        return EnumSet.of(DomainEnum.DS_SETTING_DOMAIN_NAME, DomainEnum.DS_MESSAGE_DOMAIN_NAME);
    }

    @Override
    protected boolean showInCustomPermissions() {
        return true;
    }

    @Override
    public Map<String, Long> getOwnerMap() {
        Map<String, Long> ownerMap = new HashMap<>();
        for (DsSettingSimpleDto dto : listService.getDsSettingsDtoDtoList()) {
            if (dto.getUserId() != null) {
                ownerMap.put(dto.getId(), dto.getUserId());
            }
        }
        return ownerMap;
    }
}
