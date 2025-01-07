package cz.bbn.cerberus.azure.dto;

import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;

@Getter
@Setter
public class OutlookEventDto {
    private String subject;
    private String body;
    private LocalDateTime from;
    private LocalDateTime to;
}
