package cz.bbn.cerberus.document.factory;

import cz.bbn.cerberus.document.dto.DocumentByObjectDto;
import cz.bbn.cerberus.document.persistance.entity.DocumentByObjectEntity;
import cz.bbn.cerberus.document.persistance.entity.DocumentByObjectId;

public class DocumentByObjectFactory {

    private DocumentByObjectFactory() {
    }

    public static DocumentByObjectEntity fromDto(String name, DocumentByObjectDto documentByObjectDto) {
        DocumentByObjectId documentByObjectId = new DocumentByObjectId();
        documentByObjectId.setDocumentName(name);
        documentByObjectId.setObjectId(documentByObjectDto.getObjectId());
        documentByObjectId.setObjectType(documentByObjectDto.getObjectType());
        return new DocumentByObjectEntity(documentByObjectId);
    }
}
