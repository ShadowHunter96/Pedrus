package cz.bbn.cerberus.enumeration.persistance.entity;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Entity
@Table(name = "enumeration", schema = "enums")
@Getter
@Setter
public class EnumerationEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @OneToOne
    @JoinColumn(name = "enumeration_type_id", referencedColumnName = "id")
    private EnumerationTypeEntity enumerationTypeEntity;

    private String name;
    private String description;
    private String value;
    private Boolean allowed;
    private Boolean deleted;
}
