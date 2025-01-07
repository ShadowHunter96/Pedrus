package cz.bbn.cerberus.dph;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.dph.dto.DphDto;
import cz.bbn.cerberus.dph.dto.DphFilterDto;
import cz.bbn.cerberus.dph.factory.DphFactory;
import cz.bbn.cerberus.dph.persistance.DphDao;
import cz.bbn.cerberus.dph.persistance.entity.DphEntity;
import cz.bbn.cerberus.dph.persistance.repository.DphRepository;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;

@Service
public class DphService {

    private final DphDao dphDao;
    private final DphRepository dphRepository;
    private final AppLogService appLogService;

    public DphService(DphDao dphDao, DphRepository dphRepository, AppLogService appLogService) {
        this.dphDao = dphDao;
        this.dphRepository = dphRepository;
        this.appLogService = appLogService;
    }

    public Page<DphDto> findDphDtoPage(DphFilterDto filter) {
        return dphDao.findDphPage(filter);
    }

    public DphDto getDphDto(String id) throws SystemException {
        DphEntity entity = getEntityById(id);
        return DphFactory.fromEntity(entity);
    }

    public boolean dphExists(String id) {
        return dphRepository.existsById(id);
    }

    @Transactional
    public void saveDph(DphDto dto) throws SystemException {
        if (dphExists(dto.getId())) {
            throw new SystemException(ErrorCode.ID_ALREADY_EXISTS, dto.getId());
        }
        DphEntity entity = new DphEntity();
        saveDph(entity, dto);
        appLogService.logInsert(dto, DomainEnum.DPH_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void updateDph(DphDto dto, DphDto originalDto) throws SystemException {
        DphEntity entity = getEntityById(dto.getId());
        saveDph(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.DPH_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void delete(String id) throws SystemException {
        DphEntity entity = getEntityById(id);
        entity.setAllowed(Boolean.FALSE);
        dphRepository.save(entity);
    }

    private void saveDph(DphEntity entity, DphDto dto) throws SystemException {
        List<DphEntity> defaultValueList = dphRepository.defaultValueTrueList(dto.getId());
        if (Boolean.TRUE.equals(dto.getDefaultValue()) && !defaultValueList.isEmpty()) {
            throw new SystemException(ErrorCode.DEFAULT_VALUE_ASSIGNED, defaultValueList.get(0).getId());
        }
        DphFactory.fillEntity(entity, dto);
        dphRepository.save(entity);
    }

    private DphEntity getEntityById(String id) throws SystemException {
        return dphRepository.findById(id)
                .orElseThrow(() -> new SystemException(ErrorCode.DPH_NOT_EXITS, id));
    }

    public List<DphEntity> getAllAllowed() {
        return dphRepository.findAllAllowed();
    }

    public DphDto getDefaultDph() {
        List<DphEntity> dphList = dphRepository.getAllowedDefaultList();
        if (!dphList.isEmpty()) {
            return DphFactory.fromEntity(dphList.get(0));
        }
        return null;
    }
}
