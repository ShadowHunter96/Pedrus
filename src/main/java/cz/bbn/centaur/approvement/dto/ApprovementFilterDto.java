package cz.bbn.cerberus.approvement.dto;

import cz.bbn.cerberus.approvement.enums.ApprovementState;
import cz.bbn.cerberus.approvement.enums.ApprovementType;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.time.LocalDate;
import java.util.List;
import java.util.Set;

@Getter
@Setter
public class ApprovementFilterDto {

    private Integer id;
    private Integer year;
    private LocalDate dateFrom;
    private LocalDate dateTo;
    private Set<EmployeeDto> employeeDtoSet;
    private Set<ApprovementState> stateSet;
    private Set<ApprovementType> typeSet;

    private boolean master;
    private boolean approvement;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
