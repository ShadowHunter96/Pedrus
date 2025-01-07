package cz.bbn.cerberus.project.dto;

import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class ProjectSimpleDto {

    private String id;
    private String name;
    private String subject;
    private String contract;
    private Boolean deleted;
    private UserDto userDto;

    public ProjectSimpleDto(String name) {
        this.name = name;
    }
}
