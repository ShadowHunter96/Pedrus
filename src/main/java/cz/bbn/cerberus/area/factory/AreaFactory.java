package cz.bbn.cerberus.area.factory;

import com.vaadin.flow.component.icon.VaadinIcon;
import cz.bbn.cerberus.area.dto.AreaDto;
import cz.bbn.cerberus.area.persistance.entity.AreaEntity;

public class AreaFactory {

    private AreaFactory() {
    }

    public static AreaDto fromEntity(AreaEntity entity) {
        AreaDto dto = new AreaDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setDescription(entity.getDescription());
        dto.setIcon(VaadinIcon.valueOf(entity.getIcon()));
        dto.setBadgeVariant(entity.getBadgeVariant());
        dto.setDeleted(entity.getDeleted());
        return dto;
    }

    public static void fillEntity(AreaEntity entity, AreaDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setDescription(dto.getDescription());
        entity.setIcon(dto.getIcon().name());
        entity.setBadgeVariant(dto.getBadgeVariant());
        entity.setDeleted(dto.getDeleted());
    }
}
