package cz.bbn.cerberus.technology;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.technology.dto.TechnologyDto;
import cz.bbn.cerberus.technology.dto.TechnologyFilterDto;
import cz.bbn.cerberus.technology.factory.TechnologyFactory;
import cz.bbn.cerberus.technology.persistance.TechnologyDao;
import cz.bbn.cerberus.technology.persistance.entity.TechnologyEntity;
import cz.bbn.cerberus.technology.persistance.repository.TechnologyRepository;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;

@Service
public class TechnologyService {

    private final TechnologyDao technologyDao;
    private final TechnologyRepository technologyRepository;
    private final AppLogService appLogService;

    public TechnologyService(TechnologyDao technologyDao, TechnologyRepository technologyRepository,
                             AppLogService appLogService) {
        this.technologyDao = technologyDao;
        this.technologyRepository = technologyRepository;
        this.appLogService = appLogService;
    }

    public Page<TechnologyDto> findTechnologyDtoPage(TechnologyFilterDto filter) {
        return technologyDao.findTechnologyPage(filter);
    }

    public TechnologyDto getTechnologyDto(String id) throws SystemException {
        TechnologyEntity entity = getEntityById(id);
        return TechnologyFactory.fromEntity(entity);
    }

    public boolean technologyExists(String id) {
        return technologyRepository.existsById(id);
    }

    @Transactional
    public void saveTechnology(TechnologyDto dto) throws SystemException {
        if (technologyExists(dto.getId())) {
            throw new SystemException(ErrorCode.ID_ALREADY_EXISTS, dto.getId());
        }
        TechnologyEntity entity = new TechnologyEntity();
        saveTechnology(entity, dto);
        appLogService.logInsert(dto, DomainEnum.TECHNOLOGY_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void updateTechnology(TechnologyDto dto, TechnologyDto originalDto) throws SystemException {
        TechnologyEntity entity = getEntityById(dto.getId());
        saveTechnology(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.TECHNOLOGY_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void delete(String id) throws SystemException {
        TechnologyEntity entity = getEntityById(id);
        entity.setDeleted(Boolean.TRUE);
        technologyRepository.save(entity);
        appLogService.logDelete(id, DomainEnum.TECHNOLOGY_DOMAIN_NAME.getValue());
    }

    private void saveTechnology(TechnologyEntity entity, TechnologyDto dto) {
        TechnologyFactory.fillEntity(entity, dto);
        technologyRepository.save(entity);
    }

    private TechnologyEntity getEntityById(String id) throws SystemException {
        return technologyRepository.findById(id)
                .orElseThrow(() -> new SystemException(ErrorCode.TECHNOLOGY_NOT_EXITS, id));
    }

    public List<TechnologyDto> findAllowedTechnologyDtoList() {
        return ConvertEntities.fromEntities(technologyRepository.findAllNotDeleted(), TechnologyFactory::fromEntity);
    }
}
