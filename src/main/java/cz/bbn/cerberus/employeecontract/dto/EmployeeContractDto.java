package cz.bbn.cerberus.employeecontract.dto;

import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Getter
@Setter
public class EmployeeContractDto implements Serializable {

    private String id;
    private SubjectDto ownCompany;
    private ContractTypeDto type;
    private EmployeeDto employee;
    private String linkedContractId;
    private String name;
    private LocalDate validFrom;
    private LocalDate validTo;
    private Integer reminder;
    private String contractNumber;
    private EnumerationDto state;
    private String description;
    private LocalDateTime creationDate;
    private Integer sequence;
    private Integer subsequence;
    private Boolean deleted;
    private Boolean archived;
}
