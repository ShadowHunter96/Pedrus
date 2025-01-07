package cz.bbn.cerberus.subject.dto;

import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;
import java.util.Set;

@Getter
@Setter
public class SubjectFilterDto {

    private String id;
    private String name;
    private String description;
    private String url;
    private boolean showDeleted;
    private Set<UserDto> userDtoSet;
    private Set<SubjectType> subjectTypeSet;

    private int page;
    private int size;
    private List<Sort.Order> orderList;

}
