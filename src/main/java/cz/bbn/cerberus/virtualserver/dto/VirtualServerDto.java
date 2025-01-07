package cz.bbn.cerberus.virtualserver.dto;

import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
public class VirtualServerDto implements Serializable {

    private Long id;
    private String name;
    private String os;
    private Integer cpu;
    private Integer cores;
    private Integer ram;
    private String ip;
    private UserDto owner;
    private LocalDateTime creationDate;
    private LocalDateTime requestDate;
    private VirtualServerStatus status;
    private Boolean deleted;
    private List<HddDto> hddDtoList;
    private EnumerationDto subnet;
    private String stringId;
    private VirtualServerNotificationPeriod notificationPeriod;
}
