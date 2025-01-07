package cz.bbn.cerberus.dssetting.dto;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class DsSettingFilterDto {

    private boolean showDeleted;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
