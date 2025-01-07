package cz.bbn.cerberus.area.dto;

import com.vaadin.flow.component.icon.VaadinIcon;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.vaadin.addons.badge.Badge;

import java.io.Serializable;

@Getter
@Setter
@ToString
public class AreaDto implements Serializable {

    private String id;
    private String name;
    private String description;

    private VaadinIcon icon;
    private Badge.BadgeVariant badgeVariant;
    private Boolean deleted;
}
