package cz.bbn.cerberus.employee.dto;

import cz.bbn.cerberus.commons.enums.ObjectType;
import lombok.Getter;
import lombok.Setter;

import java.util.Set;

@Getter
@Setter
public class EmployeeLinkDto {
    private String objectId;
    private ObjectType objectType;
    private Set<EmployeeDto> employeeDtoSet;
}
