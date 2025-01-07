package cz.bbn.cerberus.asset.persistance.entity;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Entity
@Table(name = "asset_by_object", schema = "sales")
@Getter
@Setter
public class AssetByObjectEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @OneToOne
    @JoinColumn(name = "asset_id", referencedColumnName = "id")
    private AssetSimpleEntity asset;

    @Column(name = "object_type")
    private String objectType;

    @Column(name = "added_to_object")
    private String addedToObjectId;
}
