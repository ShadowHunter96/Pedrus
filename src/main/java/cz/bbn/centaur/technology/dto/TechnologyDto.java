package cz.bbn.cerberus.technology.dto;

import com.vaadin.flow.component.icon.VaadinIcon;
import lombok.Getter;
import lombok.Setter;
import org.vaadin.addons.badge.Badge;

import java.io.Serializable;

@Getter
@Setter
public class TechnologyDto implements Serializable {

    private String id;
    private String name;
    private String description;

    private VaadinIcon icon;
    private Badge.BadgeVariant badgeVariant;
    private Boolean deleted;
}
