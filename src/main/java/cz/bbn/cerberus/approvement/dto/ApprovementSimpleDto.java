package cz.bbn.cerberus.approvement.dto;

import cz.bbn.cerberus.approvement.enums.ApprovementState;
import cz.bbn.cerberus.approvement.enums.ApprovementType;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDate;
import java.time.LocalDateTime;

@Getter
@Setter
public class ApprovementSimpleDto {

    private Long id;
    private LocalDate dateFrom;
    private LocalDate dateTo;
    private ApprovementType approvementType;
    private Boolean lineManageApproved;
    private Boolean superiorApproved;
    private UserDto createdUserDto;
    private UserDto ownerUserDto;
    private ApprovementState approvementState;
    private LocalDateTime created;
    private Double days;
    private ApprovementBusinessTripDto approvementBusinessTripDto;
    private Long lineManagerId;
    private String lineManagerRoleId;
    private Long superiorId;
    private String superiorRoleId;
}
