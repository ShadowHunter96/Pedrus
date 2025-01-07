package cz.bbn.cerberus.employeecontract.dto;

import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.time.LocalDate;
import java.util.List;

@Getter
@Setter
public class EmployeeContractFilterDto {

    private String id;
    private EmployeeDto employee;
    private EnumerationDto state;
    private ContractTypeDto type;
    private SubjectDto company;
    private LocalDate validTo;
    private Boolean showDeleted;
    private Boolean showArchived;
    private String linkedContract;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
