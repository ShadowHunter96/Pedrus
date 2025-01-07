package cz.bbn.cerberus.dssetting.dto;

import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.InputStream;
import java.io.Serializable;

@Getter
@Setter
@ToString
public class DsSettingDto implements Serializable {

    private String id;

    private String name;
    private String description;
    private boolean loginByCertificate;
    private String login;
    private String password;
    private InputStream certificate;
    private UserDto userDto;

    private Boolean deleted;
}
