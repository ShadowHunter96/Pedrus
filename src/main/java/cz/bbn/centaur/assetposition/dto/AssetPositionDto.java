package cz.bbn.cerberus.assetposition.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;

@Getter
@Setter
@ToString
public class AssetPositionDto implements Serializable {

    private String id;
    private String name;
    private String description;
    private String longitude;
    private String latitude;
    private Boolean deleted;
}
