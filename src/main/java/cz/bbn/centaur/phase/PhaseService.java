package cz.bbn.cerberus.phase;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.phase.dto.PhaseDto;
import cz.bbn.cerberus.phase.dto.PhaseFilterDto;
import cz.bbn.cerberus.phase.factory.PhaseFactory;
import cz.bbn.cerberus.phase.repository.PhaseDao;
import cz.bbn.cerberus.phase.repository.PhaseEntity;
import cz.bbn.cerberus.phase.repository.PhaseRepository;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;

@Service
public class PhaseService {

    private final PhaseDao phaseDao;
    private final PhaseRepository phaseRepository;
    private final AppLogService appLogService;

    public PhaseService(PhaseDao phaseDao, PhaseRepository phaseRepository, AppLogService appLogService) {
        this.phaseDao = phaseDao;
        this.phaseRepository = phaseRepository;
        this.appLogService = appLogService;
    }

    public Page<PhaseDto> findPhaseEntityPage(PhaseFilterDto filterDto) {
        return phaseDao.findPhasePage(filterDto);
    }

    public boolean phaseExists(Long id) {
        return phaseRepository.existsById(id);
    }

    @Transactional
    public void savePhase(PhaseDto dto) {
        PhaseEntity entity = new PhaseEntity();
        savePhase(entity, dto);
        appLogService.logInsert(dto, DomainEnum.PHASE_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void updatePhase(PhaseDto dto, PhaseDto originalDto) throws SystemException {
        PhaseEntity entity = getEntityById(dto.getId());
        savePhase(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.PHASE_DOMAIN_NAME.getValue());
    }

    private void savePhase(PhaseEntity entity, PhaseDto dto) {
        PhaseFactory.fillEntity(entity, dto);
        phaseRepository.save(entity);
    }

    private PhaseEntity getEntityById(Long id) throws SystemException {
        return phaseRepository.findById(id)
                .orElseThrow(() -> new SystemException(ErrorCode.PHASE_NOT_EXITS, id));
    }

    public List<PhaseDto> getPhaseList() {
        return ConvertEntities.fromEntities(phaseRepository.findAll(), PhaseFactory::fromEntity);
    }
}
