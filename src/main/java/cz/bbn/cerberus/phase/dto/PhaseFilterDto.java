package cz.bbn.cerberus.phase.dto;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class PhaseFilterDto {

    private String projectId;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
