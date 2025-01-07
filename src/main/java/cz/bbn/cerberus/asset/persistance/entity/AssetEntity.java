package cz.bbn.cerberus.asset.persistance.entity;

import cz.bbn.cerberus.asset.dto.AssetStateEnum;
import cz.bbn.cerberus.asset.dto.ProcurementEnum;
import cz.bbn.cerberus.assetposition.persistance.AssetPositionEntity;
import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.employee.persistance.entity.EmployeeEntity;
import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationEntity;
import cz.bbn.cerberus.subject.persistance.SubjectEntity;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.math.BigDecimal;
import java.time.LocalDate;

@Entity
@Table(name = "asset", schema = "backoffice")
@Getter
@Setter
public class AssetEntity {

    @Id
    private String id;

    private String name;
    private String description;

    @Column(name = "serial_number")
    private String serialNumber;

    private String type;

    @Column(name = "buy_date")
    private LocalDate buyDate;

    private BigDecimal price;

    @Column(name = "removal_date")
    private LocalDate removalDate;

    @Column(name = "quarantee_date")
    private LocalDate quaranteeDate;

    private String destination;

    @Column(name = "inventory_date")
    private LocalDate inventoryDate;

    private Boolean deleted;

    @OneToOne
    @JoinColumn(name = "asset_position_id", referencedColumnName = "id")
    private AssetPositionEntity assetPositionEntity;

    private String longitude;
    private String latitude;

    @Column(name = "pohoda_id")
    private String pohodaId;

    @OneToOne
    @JoinColumn(name = "our_company", referencedColumnName = "id")
    private SubjectEntity ourCompany;

    @Enumerated(EnumType.STRING)
    private ProcurementEnum procurement;

    @OneToOne
    @JoinColumn(name = "responsible_person", referencedColumnName = "id")
    private EmployeeEntity responsiblePerson;

    private Boolean depreciation;
    private String location;

    @OneToOne
    @JoinColumn(name = "category", referencedColumnName = "id")
    private EnumerationEntity category;

    @Enumerated(EnumType.STRING)
    private AssetStateEnum state;

    @Column(name = "currency")
    @Enumerated(EnumType.STRING)
    private AppCurrency appCurrency;

}
