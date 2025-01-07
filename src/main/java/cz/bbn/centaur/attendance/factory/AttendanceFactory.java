package cz.bbn.cerberus.attendance.factory;

import cz.bbn.cerberus.attendance.dto.AttendanceSimpleDocumentDto;
import cz.bbn.cerberus.attendance.persistance.entity.AttendanceSimpleDocumentEntity;
import cz.bbn.cerberus.user.factory.UserFactory;

public class AttendanceFactory {

    private AttendanceFactory() {
    }


    public static AttendanceSimpleDocumentDto fromEntity(AttendanceSimpleDocumentEntity entity){
        AttendanceSimpleDocumentDto dto = new AttendanceSimpleDocumentDto();
        dto.setUserDto(UserFactory.fromEntity(entity.getUserEntity()));
        dto.setName(entity.getName());
        dto.setId(entity.getId());
        dto.setDate(entity.getDate());
        dto.setDeleted(entity.getDeleted());
        return dto;
    }

}
