package cz.bbn.cerberus.employee.factory;

import cz.bbn.cerberus.employee.dto.EmployeeByObjectDto;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.employee.persistance.entity.EmployeeByObjectEntity;
import cz.bbn.cerberus.employee.persistance.entity.EmployeeEntity;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;

public class EmployeeFactory {

    private EmployeeFactory() {
    }

    public static EmployeeDto fromEntity(EmployeeEntity entity) {
        EmployeeDto dto = new EmployeeDto();
        dto.setId(entity.getId());
        dto.setFirstName(entity.getFirstName());
        dto.setLastName(entity.getLastName());
        dto.setCompanyEmail(entity.getCompanyEmail());
        dto.setPersonalEmail(entity.getPersonalEmail());
        dto.setCompanyPhoneNumber(entity.getCompanyPhoneNumber());
        dto.setPersonalPhoneNumber(entity.getPersonalPhoneNumber());
        dto.setAccountNumber(entity.getAccountNumber());
        dto.setPosition(entity.getPosition());
        dto.setStartDate(entity.getStartDate());
        dto.setActive(entity.getActive());
        dto.setDismissDate(entity.getDismissDate());
        if (entity.getLineManagerUserEntity() != null) {
            dto.setLineManagerUserDto(UserFactory.fromEntity(entity.getLineManagerUserEntity(), false));
        }
        if (entity.getSuperiorUserEntity() != null) {
            dto.setSuperiorUserDto(UserFactory.fromEntity(entity.getSuperiorUserEntity(), false));
        }
        dto.setDeleted(entity.getDeleted());
        return dto;
    }

    public static EmployeeByObjectDto fromEntity(EmployeeByObjectEntity entity) {
        EmployeeByObjectDto dto = new EmployeeByObjectDto();
        dto.setId(entity.getId());
        if (entity.getEmployeeEntity() != null) {
            dto.setEmployeeDto(fromEntity(entity.getEmployeeEntity()));
        }
        return dto;
    }

    public static void fillEntity(EmployeeEntity entity, EmployeeDto dto) {
        entity.setId(dto.getId());
        entity.setFirstName(dto.getFirstName());
        entity.setLastName(dto.getLastName());
        entity.setCompanyEmail(dto.getCompanyEmail());
        entity.setPersonalEmail(dto.getPersonalEmail());
        entity.setCompanyPhoneNumber(dto.getCompanyPhoneNumber());
        entity.setPersonalPhoneNumber(dto.getPersonalPhoneNumber());
        entity.setAccountNumber(dto.getAccountNumber());
        entity.setPosition(dto.getPosition());
        entity.setStartDate(dto.getStartDate());
        entity.setActive(dto.getActive());
        entity.setDismissDate(dto.getDismissDate());
        entity.setDeleted(dto.getDeleted());
        if (dto.getLineManagerUserDto() != null) {
            UserEntity userEntity = new UserEntity();
            UserFactory.fillEntity(userEntity, dto.getLineManagerUserDto());
            entity.setLineManagerUserEntity(userEntity);
        } else {
            entity.setLineManagerUserEntity(null);
        }
        if (dto.getSuperiorUserDto() != null) {
            UserEntity userEntity = new UserEntity();
            UserFactory.fillEntity(userEntity, dto.getSuperiorUserDto());
            entity.setSuperiorUserEntity(userEntity);
        }
    }
}
