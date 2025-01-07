package cz.bbn.cerberus.assetposition.persistance;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "asset_position", schema = "enums")
@Getter
@Setter
public class AssetPositionEntity {

    @Id
    private String id;

    private String name;
    private String description;

    private String longitude;
    private String latitude;

    private Boolean deleted;
}
