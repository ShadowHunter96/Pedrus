package cz.bbn.cerberus.asset.dto;

import lombok.Getter;
import lombok.Setter;

import java.time.LocalDate;

@Getter
@Setter
public class AssetByObjectDto {

    private Long id;
    private String assetId;
    private String name;
    private String serialNumber;
    private String type;
    private LocalDate buyDate;
    private Double price;
    private String responsiblePerson;

}
