package cz.bbn.cerberus.asset.dto;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class AssetFilterDto {

    private String id;
    private String name;
    private String serialNumber;
    private String owner;
    private String type;
    private boolean showDeleted;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
