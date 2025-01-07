package cz.bbn.cerberus.label.persistance;


import cz.bbn.cerberus.label.dto.LabelType;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "label", schema = "enums")
@Getter
@Setter
public class LabelEntity {

    @Id
    private String id;

    private String name;

    @Enumerated(EnumType.STRING)
    private LabelType type;

    private Boolean deleted;

    @Column(name = "table_values")
    private String tableValues;
}
