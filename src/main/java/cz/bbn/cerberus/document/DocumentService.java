package cz.bbn.cerberus.document;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.document.documentfileservice.DocumentFileInterface;
import cz.bbn.cerberus.document.dto.DocumentDto;
import cz.bbn.cerberus.document.dto.DocumentFileDto;
import cz.bbn.cerberus.document.dto.DocumentFilterDto;
import cz.bbn.cerberus.document.factory.DocumentByObjectFactory;
import cz.bbn.cerberus.document.factory.DocumentFactory;
import cz.bbn.cerberus.document.persistance.entity.DocumentByObjectEntity;
import cz.bbn.cerberus.document.persistance.entity.DocumentByObjectId;
import cz.bbn.cerberus.document.persistance.entity.DocumentEntity;
import cz.bbn.cerberus.document.persistance.repository.DocumentByObjectRepository;
import cz.bbn.cerberus.document.persistance.repository.DocumentDao;
import cz.bbn.cerberus.document.persistance.repository.DocumentRepository;
import cz.bbn.cerberus.documenttype.DocumentTypeService;
import cz.bbn.cerberus.documenttype.persistance.DocumentTypeEntity;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.tmatesoft.svn.core.SVNException;

import javax.transaction.Transactional;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Service
public class DocumentService {

    private final DocumentDao documentDao;
    private final DocumentRepository documentRepository;
    private final DocumentByObjectRepository documentByObjectRepository;
    private final DocumentTypeService documentTypeService;
    private final AppLogService appLogService;

    private final DocumentFileInterface documentFileInterface;

    public DocumentService(DocumentDao documentDao, DocumentRepository documentRepository,
                           DocumentByObjectRepository documentByObjectRepository,
                           DocumentTypeService documentTypeService, AppLogService appLogService,
                           DocumentFileInterface sqlFileService) {
        this.documentDao = documentDao;
        this.documentRepository = documentRepository;
        this.documentByObjectRepository = documentByObjectRepository;
        this.documentTypeService = documentTypeService;
        this.appLogService = appLogService;
        this.documentFileInterface = sqlFileService;
    }

    public Page<DocumentDto> findDocumentDtoPage(DocumentFilterDto filter) {
        return documentDao.findDocumentDtoPage(filter);
    }

    public List<String> getUsedContactPersonType(String id) {
        return documentRepository.getUsedDocumentTypeList(id);
    }

    public DocumentDto getDocumentDto(String name) throws SystemException {
        return DocumentFactory.fromEntity(getEntityByName(name));
    }

    public DocumentDto getDocumentWithFileDto(String id) throws SystemException, IOException, SVNException {
        DocumentDto documentDto = getDocumentDto(id);
        DocumentFileDto documentFileDto = documentFileInterface.getFile(id);
        documentDto.setDocumentFileDto(documentFileDto);
        return documentDto;
    }

    public DocumentDto getDocumentDtoByName(String name) {
        DocumentEntity documentEntity = documentRepository.findByName(name);
        return DocumentFactory.fromEntity(documentEntity);
    }

    public boolean documentExists(String id) {
        return documentRepository.existsById(id);
    }

    @Transactional
    public void saveDocument(DocumentDto dto) throws SystemException, IOException, SVNException {
        if (dto.getDocumentFileDto().getFileData().available() <= 0) {
            throw new SystemException(ErrorCode.DOCUMENT_FILE_IS_NOT_FILLED);
        }

        DocumentEntity documentEntity = documentRepository.findByName(dto.getName());

        if (documentEntity == null) {
            DocumentEntity entity = new DocumentEntity();

            DocumentFactory.fillEntity(entity, dto);

            documentFileInterface.saveDocument(dto.getDocumentFileDto());
            documentRepository.save(entity);

            List<DocumentByObjectEntity> documentByObjectEntityList = new ArrayList<>();
            dto.getDocumentByObjectDtoList().forEach(documentEntityDto ->
                    documentByObjectEntityList.add(DocumentByObjectFactory.fromDto(dto.getName(), documentEntityDto)));
            documentByObjectRepository.saveAll(documentByObjectEntityList);

            appLogService.logInsert(dto, DomainEnum.DOCUMENT_DOMAIN_NAME.getValue());
        } else if (documentEntity.getName().equalsIgnoreCase(dto.getName()) &&
                documentEntity.getSize().equals(dto.getSize())) {
            throw new SystemException(ErrorCode.DOCUMENT_NAME_SIZE_ALREADY_EXISTS, dto.getName());
        } else if (documentEntity.getName().equalsIgnoreCase(dto.getName())) {
            throw new SystemException(ErrorCode.DOCUMENT_NAME_EXISTS, dto.getName());
        }
    }

    @Transactional
    public void updateDocument(DocumentDto dto, DocumentDto originalDto)
            throws SystemException, IOException, SVNException {
        if (dto.getDocumentFileDto().getFileData().available() <= 0) {
            dto.setSize(originalDto.getSize());
            dto.setFileType(originalDto.getFileType());
        }
        DocumentEntity entity = documentRepository.findByName(dto.getName());

        if (entity != null && !dto.getName().equals(originalDto.getName())) {
            if (entity.getName().equalsIgnoreCase(dto.getName()) &&
                    entity.getSize().equals(dto.getSize())) {
                throw new SystemException(ErrorCode.DOCUMENT_NAME_SIZE_ALREADY_EXISTS, dto.getName());
            } else if (entity.getName().equalsIgnoreCase(dto.getName())) {
                throw new SystemException(ErrorCode.DOCUMENT_NAME_EXISTS, dto.getName());
            }
        }

        if (entity == null) {
            entity = new DocumentEntity();
        }
        DocumentFactory.fillEntity(entity, dto);

        if (dto.getDocumentFileDto().getFileData().available() > 0) {
            documentFileInterface.saveDocument(dto.getDocumentFileDto());
        }
        if (!dto.getName().equalsIgnoreCase(originalDto.getName())) {
            documentFileInterface.updateName(originalDto.getName(), dto.getName());
        }

        documentRepository.save(entity);


        if (!dto.getName().equalsIgnoreCase(originalDto.getName())) {
            List<DocumentByObjectEntity> documentByObjectEntityList =
                    documentByObjectRepository.findByIdDocumentName(originalDto.getName());
            List<DocumentByObjectEntity> documentByObjectEntityListNew = new ArrayList<>();

            documentByObjectEntityList.forEach(documentByObjectEntity ->
                    documentByObjectEntityListNew.add(
                            new DocumentByObjectEntity(
                                    new DocumentByObjectId(dto.getName(),
                                            documentByObjectEntity.getId().getObjectId(),
                                            documentByObjectEntity.getId().getObjectType())))
            );
            documentByObjectRepository.saveAll(documentByObjectEntityListNew);
            documentByObjectRepository.deleteByIdDocumentName(originalDto.getName());

            documentRepository.deleteById(originalDto.getName());
        }
        appLogService.logInsert(dto, DomainEnum.DOCUMENT_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void changeDocumentType(String newType, String oldType) throws SystemException {
        documentRepository.changeDocumentType(new DocumentTypeEntity(oldType), new DocumentTypeEntity(newType));
        documentTypeService.deleteDocumentType(oldType);
        appLogService.log("change document type", "replace documentType for all documents from "
                .concat(oldType)
                .concat(" to ")
                .concat(newType), newType);
    }

    @Transactional
    public void linkDocument(DocumentByObjectId documentByObjectId) {
        documentByObjectRepository.save(new DocumentByObjectEntity(documentByObjectId));
        appLogService.log("link document",
                "object type ".concat(documentByObjectId.getObjectType().name()).concat(" with id ")
                        .concat(documentByObjectId.getObjectId()),
                documentByObjectId.getDocumentName());
    }

    public void deleteOrUnlinkDocument(String name, String entityId,
                                       DocumentObjectEnum documentObjectEnum, boolean delete) throws SystemException {
        DocumentEntity documentEntity = getEntityByName(name);
        if (delete) {
            documentEntity.setDeleted(Boolean.TRUE);
            documentEntity.setDeletedDate(LocalDateTime.now());
            appLogService.logDelete(name, DomainEnum.DOCUMENT_DOMAIN_NAME.getValue());
        } else {
            DocumentByObjectId documentByObjectId = new DocumentByObjectId(name, entityId, documentObjectEnum);
            documentByObjectRepository.delete(new DocumentByObjectEntity(documentByObjectId));
            appLogService.log("unlink document", "", name);
        }

        documentRepository.save(documentEntity);
    }

    public void deleteOrRestoreDocument(String name, boolean restore)
            throws SystemException, IOException, SVNException {
        if (restore) {
            DocumentEntity documentEntity = getEntityByName(name);
            documentEntity.setDeleted(false);
            documentEntity.setDeletedDate(null);
            documentRepository.save(documentEntity);
            appLogService.log("document restored", "set deleted to false", name);
        } else {
            documentFileInterface.deleteFile(name);
            documentRepository.deleteById(name);
            appLogService.log("document completely deleted", "", name);
        }
    }

    private DocumentEntity getEntityByName(String name) throws SystemException {
        return documentRepository.findById(name)
                .orElseThrow(() -> new SystemException(ErrorCode.DOCUMENT_NOT_EXISTS, name));
    }

    public List<String> findDocumentNameListByContract(String objectId, DocumentObjectEnum objectType) {
        return documentByObjectRepository.findByObjectIdObjectType(objectId, objectType);
    }
}
