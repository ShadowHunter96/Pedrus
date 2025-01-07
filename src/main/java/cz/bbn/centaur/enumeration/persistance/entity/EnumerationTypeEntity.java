package cz.bbn.cerberus.enumeration.persistance.entity;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "enumeration_type", schema = "enums")
@Getter
@Setter
public class EnumerationTypeEntity {

    @Id
    private String id;

    private String name;
    private String description;

    @Column(name = "translation_key")
    private String translationKey;

    @Column(name = "permission_key")
    private String permissionKey;
}
