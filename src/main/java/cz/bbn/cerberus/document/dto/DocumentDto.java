package cz.bbn.cerberus.document.dto;

import cz.bbn.cerberus.document.DocumentObjectEnum;
import cz.bbn.cerberus.documenttype.dto.DocumentTypeDto;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
@ToString
public class DocumentDto implements Serializable {

    private String name;

    private String entityId;
    private DocumentObjectEnum documentObjectEnum;
    private List<DocumentByObjectDto> documentByObjectDtoList;

    private String fileType;
    private DocumentTypeDto documentTypeDto;
    private String documentType;

    private Long size;

    private boolean isNew;

    private DocumentFileDto documentFileDto;

    private Boolean deleted;
}
