package cz.bbn.cerberus.asset.dto;

import cz.bbn.cerberus.assetposition.dto.AssetPositionDto;
import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDate;

@Getter
@Setter
@ToString
public class AssetDto implements Serializable {

    private String id;
    private String name;
    private String description;
    private String serialNumber;
    private String type;
    private LocalDate buyDate;
    private BigDecimal price;
    private LocalDate removalDate;
    private LocalDate quaranteeDate;
    private String destination;
    private LocalDate inventoryDate;
    private Boolean deleted;

    private AssetPositionDto assetPositionDto;
    private String longitude;
    private String latitude;

    private String pohodaId;
    private SubjectDto ourCompany;
    private ProcurementEnum procurement;
    private EmployeeDto responsiblePerson;
    private Boolean depreciation;
    private String location;
    private EnumerationDto category;
    private AssetStateEnum state;

    private AppCurrency appCurrency;
}
