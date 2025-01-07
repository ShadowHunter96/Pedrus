package cz.bbn.cerberus.employee.dto;

import cz.bbn.cerberus.commons.enums.ObjectType;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class EmployeeFilterDto {

    private String id;
    private String firstName;
    private String lastName;
    private String companyEmail;
    private String companyPhoneNumber;

    private boolean showDeleted = false;

    private String objectId;
    private ObjectType objectType;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
