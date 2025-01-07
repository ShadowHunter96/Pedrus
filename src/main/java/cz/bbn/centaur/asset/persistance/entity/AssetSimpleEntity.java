package cz.bbn.cerberus.asset.persistance.entity;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.time.LocalDate;

@Entity
@Table(name = "asset", schema = "backoffice")
@Getter
@Setter
public class AssetSimpleEntity {

    @Id
    private String id;

    private String name;

    @Column(name = "serial_number")
    private String serialNumber;

    private Double price;

    @Column(name = "buy_date")
    private LocalDate buyDate;

    @Column(name = "responsible_person")
    private String responsiblePerson;

    private String type;

    private Boolean deleted;
}
