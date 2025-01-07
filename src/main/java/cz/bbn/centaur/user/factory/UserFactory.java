package cz.bbn.cerberus.user.factory;

import cz.bbn.cerberus.commons.security.AppUser;
import cz.bbn.cerberus.employee.factory.EmployeeFactory;
import cz.bbn.cerberus.employee.persistance.entity.EmployeeEntity;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.user.persistance.UserEntity;

import java.util.Optional;

public class UserFactory {

    private UserFactory() {
    }

    public static UserDto fromEntity(UserEntity userEntity) {
        return fromEntity(userEntity, true);
    }

    public static UserDto fromEntity(UserEntity userEntity, boolean loadEmployee) {
        UserDto userDto = new UserDto();
        userDto.setId(userEntity.getId());
        userDto.setName(userEntity.getName());
        userDto.setLogin(userEntity.getLogin());
        userDto.setMail(userEntity.getMail());
        userDto.setDeleted(userEntity.getDeleted());
        userDto.setSendUnreadMails(userEntity.getSendUnreadMails());
        userDto.setAzureId(userEntity.getAzureId());
        userDto.setPreferredLanguage(Optional.ofNullable(userEntity.getPreferredLanguage()).orElse("cs"));
        userDto.setAcronym(userEntity.getAcronym());

        if (userEntity.getEmployee() != null && loadEmployee) {
            userDto.setEmployee(EmployeeFactory.fromEntity(userEntity.getEmployee()));
        }

        userDto.setUserRoles(userEntity.getUserRoles());

        return userDto;
    }

    public static void fillEntity(UserEntity entity, UserDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setLogin(dto.getLogin());
        entity.setMail(dto.getMail());
        entity.setSendUnreadMails(dto.getSendUnreadMails());
        entity.setAzureId(dto.getAzureId());
        entity.setPreferredLanguage(dto.getPreferredLanguage());
        entity.setAcronym(dto.getAcronym());
        if (dto.getEmployee() != null && dto.getEmployee().getId() != null) {
            EmployeeEntity employeeEntity = new EmployeeEntity();
            EmployeeFactory.fillEntity(employeeEntity, dto.getEmployee());
            entity.setEmployee(employeeEntity);
        } else {
            entity.setEmployee(null);
        }
        entity.setUserRoles(dto.getUserRoles());
    }

    public static UserDto fromUserDto(AppUser appUser) {
        UserDto userDto = new UserDto();
        userDto.setId(appUser.getId());
        userDto.setName(appUser.getName());
        userDto.setLogin(appUser.getBid());
        userDto.setMail(appUser.getMail());
        userDto.setAzureId(appUser.getAzureId());
        userDto.setUserRoles(String.join(";", appUser.getRoleSet()));
        return userDto;
    }
}
