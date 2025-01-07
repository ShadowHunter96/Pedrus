package cz.bbn.cerberus.documenttype.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
@ToString
public class DocumentTypeDto implements Serializable {

    private String id;

    private String name;
    private String description;

    private Boolean allowed;

    private List<Long> allowedFormatsList;
}
