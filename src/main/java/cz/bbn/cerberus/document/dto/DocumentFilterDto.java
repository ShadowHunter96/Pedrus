package cz.bbn.cerberus.document.dto;

import cz.bbn.cerberus.document.DocumentObjectEnum;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class DocumentFilterDto {

    private String name;
    private String fileType;
    private String documentType;

    private String objectId;
    private DocumentObjectEnum objectType;
    private boolean showOnlyUnlinked;
    private boolean showOnlyDeleted;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
