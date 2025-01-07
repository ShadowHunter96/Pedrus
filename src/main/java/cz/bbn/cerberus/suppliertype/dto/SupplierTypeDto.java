package cz.bbn.cerberus.suppliertype.dto;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class SupplierTypeDto implements Serializable {

    private String id;
    private String name;
    private Boolean allowed;
}
