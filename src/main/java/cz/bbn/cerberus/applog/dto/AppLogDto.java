package cz.bbn.cerberus.applog.dto;

import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;


@Getter
@Setter
public class AppLogDto {

    private String action;
    private Long userId;
    private LocalDateTime date;
    private String message;
    private String appId;
    private UserDto userDto;
}
