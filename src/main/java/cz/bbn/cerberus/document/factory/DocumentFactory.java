package cz.bbn.cerberus.document.factory;

import cz.bbn.cerberus.document.dto.DocumentByObjectDto;
import cz.bbn.cerberus.document.dto.DocumentDto;
import cz.bbn.cerberus.document.persistance.entity.DocumentEntity;
import cz.bbn.cerberus.document.persistance.entity.DocumentSimpleEntity;
import cz.bbn.cerberus.documenttype.factory.DocumentTypeFactory;
import cz.bbn.cerberus.documenttype.persistance.DocumentTypeEntity;

import java.util.ArrayList;
import java.util.List;

public class DocumentFactory {

    private DocumentFactory() {
    }

    public static DocumentDto fromEntity(DocumentEntity entity) {
        DocumentDto dto = new DocumentDto();
        dto.setName(entity.getName());
        dto.setFileType(entity.getFileType());
        if (entity.getDocumentTypeEntity() != null) {
            dto.setDocumentTypeDto(DocumentTypeFactory.fromEntity(entity.getDocumentTypeEntity()));
        }

        List<DocumentByObjectDto> documentByObjectDtoList = new ArrayList<>();
        if (entity.getDocumentByObjectEntity() != null) {
            entity.getDocumentByObjectEntity().forEach(documentByObjectEntity ->
                    documentByObjectDtoList.add(new DocumentByObjectDto(documentByObjectEntity.getId().getObjectId(),
                            documentByObjectEntity.getId().getObjectType())));
            dto.setDocumentByObjectDtoList(documentByObjectDtoList);
        }
        dto.setSize(entity.getSize());
        dto.setDeleted(entity.getDeleted());
        return dto;
    }

    public static DocumentDto fromEntity(DocumentSimpleEntity entity) {
        DocumentDto dto = new DocumentDto();
        dto.setName(entity.getName());
        dto.setFileType(entity.getFileType());
        dto.setSize(entity.getSize());
        dto.setDocumentType(entity.getDocumentType());
        dto.setDeleted(entity.getDeleted());
        return dto;
    }

    public static void fillEntity(DocumentEntity entity, DocumentDto dto) {
        entity.setName(dto.getName());
        entity.setFileType(dto.getFileType());

        DocumentTypeEntity documentTypeEntity = new DocumentTypeEntity();
        DocumentTypeFactory.fillEntity(documentTypeEntity, dto.getDocumentTypeDto());
        entity.setDocumentTypeEntity(documentTypeEntity);

        entity.setSize(dto.getSize());
        entity.setDeleted(dto.getDeleted());
    }
}
