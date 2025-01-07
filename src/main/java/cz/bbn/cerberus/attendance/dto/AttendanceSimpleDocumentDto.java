package cz.bbn.cerberus.attendance.dto;

import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;

@Getter
@Setter
public class AttendanceSimpleDocumentDto {

    private Long id;
    private String name;
    private LocalDateTime date;
    private UserDto userDto;
    private Boolean deleted;
}
