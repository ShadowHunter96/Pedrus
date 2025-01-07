package cz.bbn.cerberus.suppliertype.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;

@Getter
@Setter
@ToString
public class ChangeSupplierTypeDto implements Serializable {

    private SupplierTypeDto supplierTypeDto;
}
