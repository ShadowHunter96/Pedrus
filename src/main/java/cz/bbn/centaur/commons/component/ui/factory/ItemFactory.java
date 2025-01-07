package cz.bbn.cerberus.commons.component.ui.factory;

import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.domain.ItemEntity;


public class ItemFactory {

    private ItemFactory() {
    }

    public static ItemDto fromEntity(ItemEntity itemEntity){
        return new ItemDto(itemEntity.getId(), itemEntity.getName());
    }
}
