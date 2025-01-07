package cz.bbn.cerberus.dssetting.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;

@Getter
@Setter
@ToString
public class DsSettingSimpleDto implements Serializable {

    private String id;

    private String name;
    private String description;
    private Long userId;

    private Boolean deleted;
}
