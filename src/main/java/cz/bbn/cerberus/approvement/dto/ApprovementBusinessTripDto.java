package cz.bbn.cerberus.approvement.dto;

import cz.bbn.cerberus.approvement.enums.BusinessTripTransportationType;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Getter
@Setter
public class ApprovementBusinessTripDto implements Serializable {

    private Long id;
    private SubjectDto approvementFromSubjectDto;
    private SubjectDto approvementToSubjectDto;
    private String approvementToAnother;
    private EnumerationDto purposeDto;
    private Set<BusinessTripTransportationType> businessTripTransportationTypeSet;
    private LocalDateTime interruptionFrom;
    private LocalDateTime interruptionTo;
    private ProjectDto projectDto;
    private OpportunityDto opportunityDto;
    private Set<EmployeeDto> fellowPassengers;

    public Set<EmployeeDto> getFellowPassengers(List<EmployeeDto> employeeDtoList) {
        if(fellowPassengers == null){
            return new HashSet<>();
        }
        Set<EmployeeDto> actualEmployeeSet = new HashSet<>();
        employeeDtoList.forEach(employeeDto -> {
            if(fellowPassengers.contains(employeeDto)){
                actualEmployeeSet.add(employeeDto);
            }
        });
        return actualEmployeeSet;
    }
}
