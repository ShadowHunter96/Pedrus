package cz.bbn.cerberus.technology.factory;

import com.vaadin.flow.component.icon.VaadinIcon;
import cz.bbn.cerberus.technology.dto.TechnologyDto;
import cz.bbn.cerberus.technology.persistance.entity.TechnologyEntity;

public class TechnologyFactory {

    private TechnologyFactory() {
    }

    public static TechnologyDto fromEntity(TechnologyEntity entity) {
        TechnologyDto dto = new TechnologyDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setDescription(entity.getDescription());
        dto.setIcon(VaadinIcon.valueOf(entity.getIcon()));
        dto.setBadgeVariant(entity.getBadgeVariant());
        dto.setDeleted(entity.getDeleted());
        return dto;
    }

    public static void fillEntity(TechnologyEntity entity, TechnologyDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setDescription(dto.getDescription());
        entity.setIcon(dto.getIcon().name());
        entity.setBadgeVariant(dto.getBadgeVariant());
        entity.setDeleted(dto.getDeleted());
    }
}
