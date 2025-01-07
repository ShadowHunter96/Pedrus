package cz.bbn.cerberus.custompermission.dto;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PermUserDto {

    private Long id;
    private String name;
    private boolean permanent = false;
}
