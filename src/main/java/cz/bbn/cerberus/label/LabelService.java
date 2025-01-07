package cz.bbn.cerberus.label;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.label.dto.LabelDto;
import cz.bbn.cerberus.label.dto.LabelFilterDto;
import cz.bbn.cerberus.label.factory.LabelFactory;
import cz.bbn.cerberus.label.persistance.LabelDao;
import cz.bbn.cerberus.label.persistance.LabelEntity;
import cz.bbn.cerberus.label.persistance.LabelRepository;
import cz.bbn.cerberus.labelsubject.LabelSubjectService;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;

@Service
public class LabelService {

    private final LabelRepository labelRepository;
    private final LabelDao labelDao;
    private final LabelSubjectService labelSubjectService;
    private final AppLogService appLogService;

    public LabelService(LabelRepository labelRepository, LabelDao labelDao, LabelSubjectService labelSubjectService,
                        AppLogService appLogService) {
        this.labelRepository = labelRepository;
        this.labelDao = labelDao;
        this.labelSubjectService = labelSubjectService;
        this.appLogService = appLogService;
    }

    public Page<LabelDto> findLabelDtoPage(LabelFilterDto filter) {
        return labelDao.findLabelPage(filter);
    }

    public LabelDto getLabelDto(String id) throws SystemException {
        LabelEntity entity = getEntityById(id);
        return LabelFactory.fromEntity(entity);
    }

    public boolean labelExists(String id) {
        return labelRepository.existsById(id);
    }

    @Transactional
    public void saveLabel(LabelDto dto) throws SystemException {
        if (labelExists(dto.getId())) {
            throw new SystemException(ErrorCode.ID_ALREADY_EXISTS, dto.getId());
        }
        LabelEntity entity = new LabelEntity();
        saveLabel(entity, dto);
        appLogService.logInsert(dto, DomainEnum.LABEL_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void updateLabel(LabelDto dto, LabelDto originalDto) throws SystemException {
        LabelEntity entity = getEntityById(dto.getId());
        saveLabel(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.LABEL_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void delete(String id) {
        labelRepository.deleteById(id);
        labelSubjectService.deleteByLabelId(id);
        appLogService.logDelete(id, DomainEnum.LABEL_DOMAIN_NAME.getValue());
    }

    private void saveLabel(LabelEntity entity, LabelDto dto) {
        LabelFactory.fillEntity(entity, dto);
        labelRepository.save(entity);
    }

    private LabelEntity getEntityById(String id) throws SystemException {
        return labelRepository.findById(id)
                .orElseThrow(() -> new SystemException(ErrorCode.LABEL_NOT_EXITS, id));
    }
}
