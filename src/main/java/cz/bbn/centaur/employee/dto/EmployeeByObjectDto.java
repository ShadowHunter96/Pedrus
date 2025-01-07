package cz.bbn.cerberus.employee.dto;

import cz.bbn.cerberus.employee.persistance.entity.EmployeeByObjectId;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;


@Getter
@Setter
@NoArgsConstructor
public class EmployeeByObjectDto {

    private EmployeeByObjectId id;
    private EmployeeDto employeeDto;

    public EmployeeByObjectDto(EmployeeByObjectId id) {
        this.id = id;
    }
}
