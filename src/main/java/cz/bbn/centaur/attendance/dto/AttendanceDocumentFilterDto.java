package cz.bbn.cerberus.attendance.dto;

import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.time.LocalDate;
import java.util.List;

@Getter
@Setter
public class AttendanceDocumentFilterDto {

    private UserDto userDto;
    private LocalDate from;
    private LocalDate to;
    private boolean showDeleted;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
