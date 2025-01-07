package cz.bbn.cerberus.document.factory;

import cz.bbn.cerberus.document.dto.DocumentFileDto;
import cz.bbn.cerberus.document.persistance.entity.DocumentFileEntity;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

public class DocumentFileFactory {

    private DocumentFileFactory() {
    }

    public static DocumentFileEntity fromDto(DocumentFileDto dto) throws IOException {
        DocumentFileEntity entity = new DocumentFileEntity();
        entity.setName(dto.getName());
        entity.setFile(dto.getFileData().readAllBytes());
        return entity;
    }

    public static DocumentFileDto fromEntity(DocumentFileEntity entity) {
        DocumentFileDto dto = new DocumentFileDto();
        dto.setName(entity.getName());
        if (entity.getFile() != null) {
            InputStream inputStream = new ByteArrayInputStream(entity.getFile());
            dto.setFileData(inputStream);
        }
        return dto;
    }
}
