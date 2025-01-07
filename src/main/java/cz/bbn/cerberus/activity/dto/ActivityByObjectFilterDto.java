package cz.bbn.cerberus.activity.dto;

import cz.bbn.cerberus.commons.enums.ObjectType;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class ActivityByObjectFilterDto {

    private String objectId;
    private ObjectType objectType;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
