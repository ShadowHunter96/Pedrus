package cz.bbn.cerberus.asset.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.time.LocalDate;

@Getter
@Setter
@ToString
public class AssetSimpleDto implements Serializable {

    private String id;
    private String name;
    private String serialNumber;
    private Double price;
    private LocalDate buyDate;
    private String responsiblePerson;
    private String type;
    private Boolean deleted;
}
