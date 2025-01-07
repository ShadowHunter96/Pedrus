package cz.bbn.cerberus.attendance.dto;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class AttendanceFilterDto {

    private Integer month;
    private Integer year;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
