package cz.bbn.cerberus.workreport.dto;

import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.phase.dto.PhaseDto;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Getter
@Setter
public class ProjectPhaseActivityDto {

    private List<WorkReportPickDto> pickItemList = new ArrayList<>();
    private Map<String, List<PhaseDto>> projectPhaseMap = new HashMap<>();
    private Map<String, List<EnumerationDto>> projectActivityMap = new HashMap<>();

}
