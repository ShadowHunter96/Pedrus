package cz.bbn.cerberus.labelsubject.dto;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class LabelSubjectFilterDto {

    private String subjectId;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
