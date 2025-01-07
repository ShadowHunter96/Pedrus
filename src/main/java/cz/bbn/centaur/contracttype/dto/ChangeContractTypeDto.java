package cz.bbn.cerberus.contracttype.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;

@Getter
@Setter
@ToString
public class ChangeContractTypeDto implements Serializable {

    private ContractTypeDto contractTypeDto;
}
