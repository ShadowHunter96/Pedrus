package cz.bbn.cerberus.schedulerlog.dto;

import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;

@Getter
@Setter
public class SchedulerLogDto {

    private String description;
    private LocalDateTime date;
    private String exception;
}
