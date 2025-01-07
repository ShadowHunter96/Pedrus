package cz.bbn.cerberus.documenttype;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.documenttype.dto.DocumentTypeDto;
import cz.bbn.cerberus.documenttype.factory.DocumentTypeFactory;
import cz.bbn.cerberus.documenttype.persistance.DocumentTypeEntity;
import cz.bbn.cerberus.documenttype.persistance.DocumentTypeRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;

@Service
public class DocumentTypeService {

    private final DocumentTypeRepository documentTypeRepository;
    private final AppLogService appLogService;

    public DocumentTypeService(DocumentTypeRepository documentTypeRepository, AppLogService appLogService) {
        this.documentTypeRepository = documentTypeRepository;
        this.appLogService = appLogService;
    }

    public Page<DocumentTypeDto> findDocumentTypeDtoPage(int page, int size, List<Sort.Order> sortList) {
        Page<DocumentTypeEntity> pageEntity =
                documentTypeRepository.findAll(PageRequest.of(page, size, Sort.by(sortList)));
        List<DocumentTypeDto> list = ConvertEntities
                .fromEntities(pageEntity.toList(), DocumentTypeFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(page,
                size, Sort.by(sortList)),
                pageEntity.getTotalElements());
    }

    public DocumentTypeDto getDocumentType(String id) throws SystemException {
        DocumentTypeEntity entity = getDocumentTypeEntity(id);
        return DocumentTypeFactory.fromEntity(entity);
    }

    public boolean documentTypeExists(String id) {
        return documentTypeRepository.existsById(id);
    }

    @Transactional
    public void saveDocumentType(DocumentTypeDto dto) throws SystemException {
        if (documentTypeRepository.existsById(dto.getId())) {
            throw new SystemException(ErrorCode.ID_ALREADY_EXISTS, dto.getId());
        }
        DocumentTypeEntity entity = new DocumentTypeEntity();
        saveDocumentType(entity, dto);
        appLogService.logInsert(dto, DomainEnum.DOCUMENT_TYPE_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void updateDocumentType(DocumentTypeDto dto, DocumentTypeDto originalDto) throws SystemException {
        DocumentTypeEntity entity = getDocumentTypeEntity(dto.getId());
        this.saveDocumentType(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.DOCUMENT_TYPE_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void deleteDocumentType(String id) throws SystemException {
        if (!documentTypeRepository.existsById(id)) {
            throw new SystemException(ErrorCode.DOCUMENT_TYPE_NOT_EXISTS, id);
        }
        documentTypeRepository.deleteById(id);
        appLogService.logDelete(id, DomainEnum.DOCUMENT_TYPE_DOMAIN_NAME.getValue());
    }

    public List<DocumentTypeDto> findAllAllowed(String exceptId) {
        List<DocumentTypeEntity> entityList = documentTypeRepository.findAllAllowed(exceptId);
        return ConvertEntities
                .fromEntities(entityList, DocumentTypeFactory::fromEntity);
    }

    public List<DocumentTypeDto> findAll() {
        List<DocumentTypeEntity> entityList = documentTypeRepository.findAll();
        return ConvertEntities
                .fromEntities(entityList, DocumentTypeFactory::fromEntity);
    }

    private void saveDocumentType(DocumentTypeEntity entity, DocumentTypeDto dto) {
        DocumentTypeFactory.fillEntity(entity, dto);
        documentTypeRepository.save(entity);
    }

    private DocumentTypeEntity getDocumentTypeEntity(String id) throws SystemException {
        return documentTypeRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.DOCUMENT_TYPE_NOT_EXISTS, id));
    }
}
