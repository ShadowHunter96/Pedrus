package cz.bbn.cerberus.areatechnologysign.persistance;

import cz.bbn.cerberus.area.persistance.entity.AreaEntity;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.technology.persistance.entity.TechnologyEntity;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Entity
@Getter
@Setter
@Table(name = "area_technology_sign", schema = "enums")
@NoArgsConstructor
public class AreaTechnologySignEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "object_type")
    @Enumerated(EnumType.STRING)
    private ObjectType objectType;

    @Column(name = "object_id")
    private String objectId;

    @OneToOne
    @JoinColumn(name = "area_id", referencedColumnName = "id")
    private AreaEntity areaEntity;

    @OneToOne
    @JoinColumn(name = "technology_id", referencedColumnName = "id")
    private TechnologyEntity technologyEntity;

}
