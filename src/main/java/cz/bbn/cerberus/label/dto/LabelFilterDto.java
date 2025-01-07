package cz.bbn.cerberus.label.dto;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class LabelFilterDto {

    private String id;
    private String name;
    private boolean showDeleted;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
