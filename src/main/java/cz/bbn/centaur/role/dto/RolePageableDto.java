package cz.bbn.cerberus.role.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
@AllArgsConstructor
public class RolePageableDto {

    private List<RoleDto> roleDtoList;
    private long count;
}
