package cz.bbn.cerberus.virtualserver.dto;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class HddDto {

    private Long id;
    private String name;
    private Integer size;
    private Long virtualServerId;
}
