package cz.bbn.cerberus.enumeration;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationFilterDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationTypeDto;
import cz.bbn.cerberus.enumeration.factory.EnumerationFactory;
import cz.bbn.cerberus.enumeration.persistance.EnumerationDao;
import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationEntity;
import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationTypeEntity;
import cz.bbn.cerberus.enumeration.persistance.repository.EnumerationRepository;
import cz.bbn.cerberus.enumeration.persistance.repository.EnumerationTypeRepository;
import cz.bbn.cerberus.listconfiguration.ListService;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;

@Service
public class EnumerationService {

    private final EnumerationTypeRepository enumerationTypeRepository;
    private final EnumerationDao enumerationDao;
    private final EnumerationRepository enumerationRepository;
    private final AppLogService appLogService;
    private final ListService listService;

    public EnumerationService(EnumerationTypeRepository enumerationTypeRepository, EnumerationDao enumerationDao,
                              EnumerationRepository enumerationRepository, AppLogService appLogService,
                              ListService listService) {
        this.enumerationTypeRepository = enumerationTypeRepository;
        this.enumerationDao = enumerationDao;
        this.enumerationRepository = enumerationRepository;
        this.appLogService = appLogService;
        this.listService = listService;
    }

    public List<EnumerationDto> getEnumerationDtoList(String enumerationTypeId) {
        List<EnumerationEntity> enumerationTypeEntityList =
                enumerationRepository.findByEnumerationTypeEntityId(enumerationTypeId);
        return ConvertEntities
                .fromEntities(enumerationTypeEntityList, EnumerationFactory::fromEntity);
    }

    public List<EnumerationDto> getEnumerationDtoListByTypeNotDeletedAllowed(String enumerationTypeId) {
        List<EnumerationEntity> enumerationTypeEntityList =
                enumerationRepository.getAllowedNotDeletedByType(enumerationTypeId);
        return ConvertEntities
                .fromEntities(enumerationTypeEntityList, EnumerationFactory::fromEntity);
    }

    public List<EnumerationTypeDto> getEnumerationTypeDtoList() {
        List<EnumerationTypeEntity> enumerationTypeEntityList = enumerationTypeRepository.findAll();
        return ConvertEntities
                .fromEntities(enumerationTypeEntityList, EnumerationFactory::fromEntity);
    }

    public Page<EnumerationDto> findEnumerationDtoPage(EnumerationFilterDto enumerationFilterDto) {
        return enumerationDao.findEnumerationPage(enumerationFilterDto);
    }

    public EnumerationDto getEnumerationDto(Long id) throws SystemException {
        return EnumerationFactory.fromEntity(getEntityById(id));
    }

    public boolean enumerationExists(Long id) {
        return enumerationRepository.existsById(id);
    }

    public List<EnumerationDto> getDefaultValueTrueList(Long id, String enumerationTypeId) {
        List<EnumerationEntity> enumerationEntityList =
                enumerationRepository.getByDefaultValueTrueList(id, enumerationTypeId);
        return ConvertEntities.fromEntities(enumerationEntityList, EnumerationFactory::fromEntity);
    }

    @Transactional
    public Long saveEnumeration(EnumerationDto dto) {
        EnumerationEntity entity = new EnumerationEntity();
        Long id = saveEnumeration(entity, dto);
        appLogService.logInsert(dto, DomainEnum.ENUMERATION_DOMAIN_NAME.getValue());
        return id;
    }

    @Transactional
    public void updateEnumeration(EnumerationDto dto, EnumerationDto originalDto) throws SystemException {
        EnumerationEntity entity = getEntityById(dto.getId());
        this.saveEnumeration(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.ENUMERATION_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void deleteEnumeration(Long id) throws SystemException {
        EnumerationEntity entity = getEntityById(id);
        entity.setDeleted(Boolean.TRUE);
        enumerationRepository.save(entity);
        appLogService.logDelete(String.valueOf(id), DomainEnum.ENUMERATION_DOMAIN_NAME.getValue());
        listService.reloadEnumerationList();
    }

    private Long saveEnumeration(EnumerationEntity entity, EnumerationDto dto) {
        EnumerationFactory.fillEntity(entity, dto);
        Long id = enumerationRepository.save(entity).getId();
        listService.reloadEnumerationList();
        return id;
    }

    private EnumerationEntity getEntityById(Long id) throws SystemException {
        return enumerationRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.ENUMERATION_NOT_EXISTS, id));
    }
}
