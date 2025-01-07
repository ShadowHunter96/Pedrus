package cz.bbn.cerberus.user.dto;

import cz.bbn.cerberus.employee.dto.EmployeeDto;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serializable;
import java.util.Set;


@Getter
@Setter
@NoArgsConstructor
@EqualsAndHashCode
public class UserDto implements Serializable {

    private Long id;
    private String login;
    private String name;
    private String mail;
    private Boolean deleted;
    private Boolean sendUnreadMails;
    private String azureId;
    private EmployeeDto employee;
    private Set<UserActiveRoleDto> userActiveRoleDtoSet;
    private String preferredLanguage;
    private String acronym;
    private String userRoles;

    public UserDto(String name) {
        this.name = name;
        this.id = 0L;
    }

    public UserDto(Long id) {
        this.id = id;
    }

    public String getStringId() {
        return String.valueOf(id);
    }
}
