package cz.bbn.cerberus.dph.persistance.entity;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "dph", schema = "enums")
@Getter
@Setter
public class DphEntity {

    @Id
    private String id;

    private String name;
    private Double value;
    private Boolean allowed;

    @Column(name = "default_value")
    private Boolean defaultValue;
}
