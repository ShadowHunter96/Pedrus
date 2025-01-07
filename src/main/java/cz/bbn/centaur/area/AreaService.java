package cz.bbn.cerberus.area;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.area.dto.AreaDto;
import cz.bbn.cerberus.area.dto.AreaFilterDto;
import cz.bbn.cerberus.area.factory.AreaFactory;
import cz.bbn.cerberus.area.persistance.AreaDao;
import cz.bbn.cerberus.area.persistance.entity.AreaEntity;
import cz.bbn.cerberus.area.persistance.repository.AreaRepository;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;

@Service
public class AreaService {

    private final AreaDao areaDao;
    private final AreaRepository areaRepository;
    private final AppLogService appLogService;

    public AreaService(AreaDao areaDao, AreaRepository areaRepository, AppLogService appLogService) {
        this.areaDao = areaDao;
        this.areaRepository = areaRepository;
        this.appLogService = appLogService;
    }

    public Page<AreaDto> findAreaDtoPage(AreaFilterDto filter) {
        return areaDao.findAreaPage(filter);
    }

    public AreaDto getAreaDto(String id) throws SystemException {
        AreaEntity entity = getEntityById(id);
        return AreaFactory.fromEntity(entity);
    }

    public boolean areaExists(String id) {
        return areaRepository.existsById(id);
    }

    @Transactional
    public void saveArea(AreaDto dto) throws SystemException {
        if (areaExists(dto.getId())) {
            throw new SystemException(ErrorCode.ID_ALREADY_EXISTS, dto.getId());
        }
        AreaEntity entity = new AreaEntity();
        saveArea(entity, dto);
        appLogService.logInsert(dto, DomainEnum.AREA_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void updateArea(AreaDto dto, AreaDto originalDto) throws SystemException {
        AreaEntity entity = getEntityById(dto.getId());
        saveArea(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.AREA_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void delete(String id) throws SystemException {
        AreaEntity entity = getEntityById(id);
        entity.setDeleted(Boolean.TRUE);
        areaRepository.save(entity);
        appLogService.logDelete(id, DomainEnum.AREA_DOMAIN_NAME.getValue());
    }


    private void saveArea(AreaEntity entity, AreaDto dto) {
        AreaFactory.fillEntity(entity, dto);
        areaRepository.save(entity);
    }

    private AreaEntity getEntityById(String id) throws SystemException {
        return areaRepository.findById(id)
                .orElseThrow(() -> new SystemException(ErrorCode.AREA_NOT_EXITS, id));
    }

    public List<AreaDto> findAllNotDeletedAreaDtoList() {
        return ConvertEntities.fromEntities(areaRepository.findAllNotDeleted(), AreaFactory::fromEntity);
    }
}
