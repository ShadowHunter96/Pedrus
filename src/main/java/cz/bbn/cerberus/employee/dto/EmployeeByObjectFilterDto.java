package cz.bbn.cerberus.employee.dto;

import cz.bbn.cerberus.commons.enums.ObjectType;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class EmployeeByObjectFilterDto {

    private EmployeeActive employeeActive;
    private String objectId;
    private ObjectType objectType;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
