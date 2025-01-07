package cz.bbn.cerberus.attendance.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDate;

@Getter
@Setter
@AllArgsConstructor
public class DayDto {

    private LocalDate date;
    private boolean weekend;
    private boolean holiday;

    public DayDto(LocalDate date) {
        this.date = date;
    }
}
