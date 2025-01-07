package cz.bbn.cerberus.contactperson.factory;

import cz.bbn.cerberus.contactperson.dto.ContactPersonByObjectDto;
import cz.bbn.cerberus.contactperson.dto.ContactPersonObjectTypeEnum;
import cz.bbn.cerberus.contactperson.persistance.entity.ContactPersonByObjectEntity;
import cz.bbn.cerberus.phoneprefix.factory.PhonePrefixFactory;

public class ContactPersonByObjectFactory {

    private ContactPersonByObjectFactory() {
    }

    public static ContactPersonByObjectDto fromEntity(ContactPersonByObjectEntity entity) {
        ContactPersonByObjectDto dto = new ContactPersonByObjectDto();
        dto.setId(entity.getContactPerson().getId());
        dto.setFirstName(entity.getContactPerson().getFirstName());
        dto.setLastName(entity.getContactPerson().getLastName());
        if (entity.getContactPerson().getPhonePrefixEntity() != null) {
            dto.setPhonePrefixDto(PhonePrefixFactory.getEntity(entity.getContactPerson().getPhonePrefixEntity()));
        }
        dto.setEmail(entity.getContactPerson().getEmail());
        dto.setPhone(entity.getContactPerson().getPhone());
        dto.setAddedObjectId(entity.getAddedToObjectId());
        dto.setObjectType(ContactPersonObjectTypeEnum.valueOf(entity.getObjectType()));
        return dto;
    }
}
