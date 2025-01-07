package cz.bbn.cerberus.opportunity.dto;


import cz.bbn.cerberus.commons.enums.PctValues;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.time.LocalDate;
import java.util.List;
import java.util.Set;

@Getter
@Setter
public class OpportunityDtoFilter {

    private String name;
    private boolean showDeleted;
    private String subjectId;
    private Set<SubjectDto> subjectDtoSet;
    private Set<OpportunityState> stateSet;
    private Set<PctValues> progressSet;
    private Set<PctValues> successChanceSet;
    private LocalDate startDateFrom;
    private LocalDate startDateTo;
    private boolean onlyEditPermission;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
