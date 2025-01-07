package cz.bbn.cerberus.technology.persistance.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.vaadin.addons.badge.Badge;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "technology", schema = "enums")
@Getter
@Setter
@NoArgsConstructor
public class TechnologyEntity {

    @Id
    private String id;

    private String name;
    private String description;

    private String icon;

    @Column(name = "color_variant")
    @Enumerated(EnumType.STRING)
    private Badge.BadgeVariant badgeVariant;

    private Boolean deleted;
}
