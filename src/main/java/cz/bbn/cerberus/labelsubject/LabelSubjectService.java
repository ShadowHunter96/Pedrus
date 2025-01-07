package cz.bbn.cerberus.labelsubject;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.labelsubject.dto.LabelSubjectDto;
import cz.bbn.cerberus.labelsubject.dto.LabelSubjectFilterDto;
import cz.bbn.cerberus.labelsubject.factory.LabelSubjectFactory;
import cz.bbn.cerberus.labelsubject.persistance.LabelSubjectDao;
import cz.bbn.cerberus.labelsubject.persistance.LabelSubjectEntity;
import cz.bbn.cerberus.labelsubject.persistance.LabelSubjectRepository;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;

@Service
public class LabelSubjectService {

    private final LabelSubjectDao labelSubjectDao;
    private final LabelSubjectRepository labelSubjectRepository;
    private final AppLogService appLogService;

    public LabelSubjectService(LabelSubjectDao labelSubjectDao, LabelSubjectRepository labelSubjectRepository,
                               AppLogService appLogService) {
        this.labelSubjectDao = labelSubjectDao;
        this.labelSubjectRepository = labelSubjectRepository;
        this.appLogService = appLogService;
    }

    public Page<LabelSubjectDto> findLabelSubjectDtoPage(LabelSubjectFilterDto labelSubjectFilterDto) {
        return labelSubjectDao.findLabelPage(labelSubjectFilterDto);
    }

    public boolean labelExists(Long id) {
        return labelSubjectRepository.existsById(id);
    }

    public boolean subjectLabelExist(String subjectId, String labelId) {
        return labelSubjectRepository.existsBySubjectIdAndLabelEntityId(subjectId, labelId);
    }

    @Transactional
    public void saveLabelSubject(LabelSubjectDto dto) {
        LabelSubjectEntity entity = LabelSubjectFactory.fillEntity(dto);
        labelSubjectRepository.save(entity);
        appLogService.log("Add label to subject", dto.toString(), dto.getSubjectId());
    }

    @Transactional
    public void deleteLabelSubject(Long id, String subjectId) throws SystemException {
        if (!labelExists(id)) {
            throw new SystemException(ErrorCode.LABEL_SUBJECT_NOT_EXITS);
        }
        labelSubjectRepository.deleteById(id);
        appLogService.log("Delete label from subject", "label id: ".concat(String.valueOf(id)), subjectId);
    }

    @Transactional
    public void deleteByLabelId(String labelId) {
        labelSubjectRepository.deleteByLabelEntityId(labelId);
    }
}
