package cz.bbn.cerberus.subject.dto;

import cz.bbn.cerberus.adis.AdisReliable;
import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.suppliertype.dto.SupplierTypeDto;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

@Getter
@Setter
@ToString
public class SubjectDto implements Serializable {

    private String id;
    private String name;
    private String description;
    private String url;

    private String court;
    private String fileNumber;
    private String register;

    private String ico;
    private String companyName;
    private String lawForm;

    private String address;
    private String enlistDate;

    private String capital;
    private BigDecimal capitalDecimal;

    private String companions;

    private String dic;
    private AdisReliable reliable;
    private String unreliableFrom;
    private String standardAccount;
    private String nonStandardAccount;

    private UserDto userDto;

    private Boolean localSubject;
    private Boolean customer;
    private Boolean supplier;
    private Boolean ownCompany;

    private SupplierTypeDto supplierType;

    private Boolean deleted;

    private AppCurrency appCurrency;

    private LocalDateTime lastUpdateFromAres;
}
