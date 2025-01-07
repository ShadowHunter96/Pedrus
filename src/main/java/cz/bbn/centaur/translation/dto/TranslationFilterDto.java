package cz.bbn.cerberus.translation.dto;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class TranslationFilterDto {

    private Long id;
    private String lang;
    private String key;
    private String value;
    private Boolean showEmpty;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
