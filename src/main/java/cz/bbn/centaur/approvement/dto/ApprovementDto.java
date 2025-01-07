package cz.bbn.cerberus.approvement.dto;

import cz.bbn.cerberus.approvement.enums.ApprovementState;
import cz.bbn.cerberus.approvement.enums.ApprovementType;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
public class ApprovementDto implements Serializable {
    private Long id;
    private LocalDate dateFrom;
    private LocalDate dateTo;
    private ApprovementType approvementType;
    private Boolean lineManageApproved;
    private Boolean superiorApproved;
    private List<ApprovementProjectEmployeeDto> approvementProjectEmployeeDtoList;
    private ApprovementState approvementState;
    private LocalDateTime created;
    private UserDto ownerUserDto;
    private UserDto createdUserDto;
    private String lineManageNote;
    private String superiorNote;
    private LocalDateTime lineManageApprovedDate;
    private LocalDateTime superiorApprovedDate;
    private UserDto lineManagerUserDto;
    private UserDto superiorUserDto;
    private RoleDto lineManagerRole;

    private ProjectDto firstProjectDto;
    private EmployeeDto firstEmployeeDto;
    private boolean lMSuperiorEquals;

    private String note;
    private Boolean halfDay;

    private Double days;
    private ApprovementBusinessTripDto approvementBusinessTripDto;

    private EnumerationDto enumeration;

    private EmployeeDto createdForEmployeeDto;
}
