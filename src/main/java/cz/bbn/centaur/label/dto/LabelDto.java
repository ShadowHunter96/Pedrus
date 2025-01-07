package cz.bbn.cerberus.label.dto;

import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
@ToString
public class LabelDto implements Serializable {

    private String id;
    private String name;
    private LabelType type;
    private Boolean deleted;
    private List<ItemDto> tableValueList;
}
