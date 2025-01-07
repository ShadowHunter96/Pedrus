package cz.bbn.cerberus.commons.component.ui.domain;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.List;

@Getter
@AllArgsConstructor
public class ItemsDto<T> {
    private List<T> dtoList;
    private int count;
}
