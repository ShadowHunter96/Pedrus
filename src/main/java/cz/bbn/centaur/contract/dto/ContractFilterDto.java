package cz.bbn.cerberus.contract.dto;

import cz.bbn.cerberus.area.dto.AreaDto;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.technology.dto.TechnologyDto;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.time.LocalDate;
import java.util.List;
import java.util.Set;

@Getter
@Setter
public class ContractFilterDto {

    private SubjectDto customerDto;
    private Set<EnumerationDto> contractState;
    private ContractTypeDto contractTypeDto;
    private SubjectDto contractPartyDto;
    private String id;
    private String name;
    private boolean showEmptyValidityStart;
    private boolean onlyEditPermission;

    private LocalDate validityStartFrom;
    private LocalDate validityStartTo;
    private LocalDate endDateFrom;
    private LocalDate endDateTo;
    private LocalDate effectiveStartFrom;
    private LocalDate effectiveStartTo;

    private String parentContractId;
    private UserDto userDto;
    private Set<AreaDto> areaDtoSet;
    private Set<TechnologyDto> technologyDtoSet;

    private ContractInternalType contractInternalType;

    private boolean showDeleted = false;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
    private Set<ContractInternalType> typeSet;
}
