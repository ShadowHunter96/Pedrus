package cz.bbn.cerberus.schedulerlog.dto;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.time.LocalDate;
import java.util.List;

@Getter
@Setter
public class SchedulerLogFilterDto {

    private String description;
    private LocalDate date;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
