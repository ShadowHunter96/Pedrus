package cz.bbn.cerberus.dssetting.persistance;


import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "ds_setting", schema = "security")
@Getter
@Setter
public class DsSettingSimpleEntity {

    @Id
    private String id;

    private String name;
    private String description;

    @Column(name = "user_id")
    private Long userId;

    private Boolean deleted;
}
