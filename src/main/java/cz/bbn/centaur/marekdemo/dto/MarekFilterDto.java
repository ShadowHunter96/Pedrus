package cz.bbn.cerberus.marekdemo.dto;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

/**
 * Created by marek.vu on 09.10.2023.
 */

@Getter
@Setter
public class MarekFilterDto {

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
