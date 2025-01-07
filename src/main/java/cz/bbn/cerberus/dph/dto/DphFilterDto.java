package cz.bbn.cerberus.dph.dto;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class DphFilterDto {

    private String id;
    private String name;
    private boolean showNotAllowed;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
