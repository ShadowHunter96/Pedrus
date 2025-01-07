package cz.bbn.cerberus.dph.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;

@Getter
@Setter
@ToString
public class DphDto implements Serializable {

    private String id;
    private String name;
    private Double value;
    private String description;

    private Boolean allowed;
    private Boolean defaultValue;
}
