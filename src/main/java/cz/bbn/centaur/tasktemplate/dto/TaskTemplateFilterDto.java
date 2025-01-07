package cz.bbn.cerberus.tasktemplate.dto;

import cz.bbn.cerberus.commons.enums.ObjectType;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class TaskTemplateFilterDto {

    private Long id;
    private String search;

    private Long owner;
    private String role;

    private boolean showMine;
    private boolean showDeleted;

    private ObjectType objectType;
    private String objectId;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
