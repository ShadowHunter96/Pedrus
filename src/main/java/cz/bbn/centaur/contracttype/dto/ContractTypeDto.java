package cz.bbn.cerberus.contracttype.dto;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class ContractTypeDto implements Serializable {

    private String id;
    private String name;
    private String description;
    private Boolean allowed;
    private Boolean sales;
    private Boolean supplierCo;
    private Boolean operational;
    private Boolean employeeCo;
    private Boolean connectionRequired;
}
