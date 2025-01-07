package cz.bbn.cerberus.applog.dto;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class AppLogFilterDto {

    private String action;
    private Long userId;
    private String message;
    private String appId;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
