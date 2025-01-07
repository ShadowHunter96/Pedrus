package cz.bbn.cerberus.document.dto;


import cz.bbn.cerberus.document.DocumentObjectEnum;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import java.io.Serializable;

@Getter
@Setter
@AllArgsConstructor
public class DocumentByObjectDto implements Serializable {

    private String objectId;

    @Enumerated(EnumType.STRING)
    private DocumentObjectEnum objectType;
}
