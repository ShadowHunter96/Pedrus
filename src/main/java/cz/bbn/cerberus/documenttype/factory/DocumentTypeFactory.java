package cz.bbn.cerberus.documenttype.factory;

import cz.bbn.cerberus.documenttype.dto.DocumentTypeDto;
import cz.bbn.cerberus.documenttype.persistance.DocumentTypeEntity;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.stream.Collectors;

public class DocumentTypeFactory {

    private DocumentTypeFactory() {
    }

    public static DocumentTypeDto fromEntity(DocumentTypeEntity entity) {
        DocumentTypeDto dto = new DocumentTypeDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setDescription(entity.getDescription());
        dto.setAllowed(entity.getAllowed());
        if (entity.getAllowedFormats() != null) {
            dto.setAllowedFormatsList(Arrays.stream(entity.getAllowedFormats().split(","))
                    .map((String e) -> {
                        if (e != null && !e.isEmpty()) {
                            return Long.parseLong(e);
                        }
                        return null;
                    })
                    .collect(Collectors.toList()));
        }
        return dto;
    }

    public static void fillEntity(DocumentTypeEntity entity, DocumentTypeDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setDescription(dto.getDescription());
        entity.setAllowed(dto.getAllowed());
        entity.setAllowedFormats(StringUtils.join(dto.getAllowedFormatsList(), ","));
    }
}
