package cz.bbn.cerberus.approvement.dto;

import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.project.dto.ProjectDto;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;


@Getter
@Setter
public class ApprovementProjectEmployeeDto implements Serializable {

    private Long id;
    private Long approvementId;
    private ProjectDto projectDto;
    private EmployeeDto employeeDto;
}
