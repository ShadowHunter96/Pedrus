package cz.bbn.cerberus.workreport.dto;

import cz.bbn.cerberus.approvement.dto.ApprovementDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.phase.dto.PhaseDto;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.time.LocalDate;

@Getter
@Setter
public class WorkReportDto implements Serializable {

    private Long id;
    private LocalDate date;
    private String itemId;
    private WorkReportPickDto pickDto;
    private PhaseDto phaseDto;
    private EnumerationDto activity;
    private Double duration;
    private String description;
    private String employeeId;
    private ApprovementDto approvementDto;
}
