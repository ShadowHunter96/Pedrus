package cz.bbn.cerberus.contactpersontype.factory;

import cz.bbn.cerberus.contactpersontype.dto.ContactPersonTypeDto;
import cz.bbn.cerberus.contactpersontype.persistence.ContactPersonTypeEntity;

public class ContactPersonTypeFactory {

    private ContactPersonTypeFactory() {
    }

    public static ContactPersonTypeDto fromEntity(ContactPersonTypeEntity entity){
        ContactPersonTypeDto dto = new ContactPersonTypeDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setAllowed(entity.getAllowed());
        return dto;
    }

    public static void fillEntity(ContactPersonTypeEntity entity, ContactPersonTypeDto dto){
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setAllowed(dto.getAllowed());
    }
}
