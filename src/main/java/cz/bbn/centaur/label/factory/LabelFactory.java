package cz.bbn.cerberus.label.factory;

import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.label.dto.LabelDto;
import cz.bbn.cerberus.label.persistance.LabelEntity;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class LabelFactory {

    private LabelFactory() {
    }

    public static LabelDto fromEntity(LabelEntity entity) {
        LabelDto dto = new LabelDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setType(entity.getType());
        dto.setDeleted(entity.getDeleted());
        if (entity.getTableValues() == null) {
            dto.setTableValueList(new ArrayList<>());
        } else {
            String[] values = entity.getTableValues().split(";");
            List<ItemDto> itemDtoList = new ArrayList<>();
            Arrays.stream(values).toList().forEach(s -> itemDtoList.add(new ItemDto(s)));
            dto.setTableValueList(itemDtoList);
        }
        return dto;
    }

    public static void fillEntity(LabelEntity entity, LabelDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setType(dto.getType());
        entity.setDeleted(dto.getDeleted());
        if (dto.getTableValueList() != null) {
            String values = "";
            for (ItemDto itemDto : dto.getTableValueList()) {
                values += itemDto.getName().concat(";");
            }
            entity.setTableValues(values);
        }
    }
}
