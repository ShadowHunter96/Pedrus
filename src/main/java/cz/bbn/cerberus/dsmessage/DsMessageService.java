package cz.bbn.cerberus.dsmessage;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.dsmessage.dto.DsMessageDto;
import cz.bbn.cerberus.dsmessage.dto.DsMessageFilterDto;
import cz.bbn.cerberus.dsmessage.dto.DsMessageSimpleDto;
import cz.bbn.cerberus.dsmessage.factory.DsMessageFactory;
import cz.bbn.cerberus.dsmessage.persistance.dao.DsMessageDao;
import cz.bbn.cerberus.dsmessage.persistance.entity.DsMessageEntity;
import cz.bbn.cerberus.dsmessage.persistance.repository.DsAttachementRepository;
import cz.bbn.cerberus.dsmessage.persistance.repository.DsMessageRepository;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;

@Service
public class DsMessageService {

    private final DsMessageRepository dsMessageRepository;
    private final DsMessageDao dsMessageDao;
    private final AppLogService appLogService;
    private final DsAttachementRepository attachementRepository;

    public DsMessageService(DsMessageRepository dsMessageRepository, DsMessageDao dsMessageDao,
                            AppLogService appLogService, DsAttachementRepository attachementRepository) {
        this.dsMessageRepository = dsMessageRepository;
        this.dsMessageDao = dsMessageDao;
        this.appLogService = appLogService;
        this.attachementRepository = attachementRepository;
    }


    public Page<DsMessageSimpleDto> findDsMessageDtoPage(DsMessageFilterDto filter) {
        return dsMessageDao.findDsMessageDtoPage(filter);
    }

    public DsMessageDto getMessageDto(Long id) throws SystemException {
        return DsMessageFactory.fromEntity(getDsMessageEntity(id));
    }

    public List<String> getRecipientList() {
        return dsMessageRepository.findRecieverList();
    }

    public List<String> getSenderNameList() {
        return dsMessageRepository.findSenderNameList();
    }

    public boolean dsMessageExists(Long id) {
        return dsMessageRepository.existsById(id);
    }

    public byte[] getAttachementFile(Long id) {
        return attachementRepository.getFileById(id);
    }

    @Transactional
    public void updateViewed(Long id) {
        dsMessageRepository.updateViewed(id);
        appLogService.log("Ds update viewed", "update viewed in ds: ".concat(String.valueOf(id)), String.valueOf(id));
    }

    @Transactional
    public void deleteDsMessage(Long id) throws SystemException {
        if (!dsMessageExists(id)) {
            throw new SystemException(ErrorCode.DS_MESSAGE_NOT_EXISTS, id);
        }
        DsMessageEntity entity = getDsMessageEntity(id);
        entity.setDeleted(!entity.getDeleted());
        dsMessageRepository.save(entity);
        appLogService.logDelete(String.valueOf(id), DomainEnum.DS_MESSAGE_DOMAIN_NAME.getValue());
    }

    private DsMessageEntity getDsMessageEntity(Long id) throws SystemException {
        return dsMessageRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.DOCUMENT_TYPE_NOT_EXISTS, id));
    }
}
