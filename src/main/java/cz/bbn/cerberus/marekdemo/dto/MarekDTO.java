package cz.bbn.cerberus.marekdemo.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;

import javax.validation.constraints.NotNull;

/**
 * Created by marek.vu on 04.10.2023.
 */
@Getter
@Setter
@NoArgsConstructor
public class MarekDTO {
    private long id;
    @NotNull
    private String name;
    @NotNull
    private String description;



    private Map<String, List<ContractDto>> getContractMap(List<SubjectDto> customerList,
                                                          List<ContractDto> contractList) {
        Map<String, List<ContractDto>> toReturnMap = new HashMap<>();
        Map<String, List<ContractDto>> tempMap = new HashMap<>();
        for (ContractDto contractDto : contractList) {
            if (contractDto.getSubjectDto() != null) {
                List<ContractDto> tempList;
                if (tempMap.containsKey(contractDto.getSubjectDto().getId())) {
                    tempList = tempMap.get(contractDto.getSubjectDto().getId());
                } else {
                    tempList = new ArrayList<>();
                }
                tempList.add(contractDto);
                tempMap.put(contractDto.getSubjectDto().getId(), tempList);
            }
        }
        for (SubjectDto subjectDto : customerList) {
            if (tempMap.containsKey(subjectDto.getId())) {
                List<ContractDto> contractDtoList = tempMap.get(subjectDto.getId());
                contractDtoList.sort(Comparator.comparing(ContractDto::getName));
                toReturnMap.put(subjectDto.getId(), contractDtoList);
            } else {
                toReturnMap.put(subjectDto.getId(), new ArrayList<>());
            }
        }
        return toReturnMap;
    }
}
