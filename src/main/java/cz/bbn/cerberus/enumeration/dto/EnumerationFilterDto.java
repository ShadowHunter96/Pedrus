package cz.bbn.cerberus.enumeration.dto;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class EnumerationFilterDto {

    private String id;
    private String name;
    private boolean showNotAllowed;
    private boolean showDeleted;
    private String enumerationTypeId;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
