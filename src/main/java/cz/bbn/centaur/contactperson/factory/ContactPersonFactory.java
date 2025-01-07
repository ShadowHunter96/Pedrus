package cz.bbn.cerberus.contactperson.factory;

import cz.bbn.cerberus.contactperson.dto.ContactPersonDto;
import cz.bbn.cerberus.contactperson.persistance.entity.ContactPersonEntity;
import cz.bbn.cerberus.contactpersontype.dto.ContactPersonTypeDto;
import cz.bbn.cerberus.contactpersontype.factory.ContactPersonTypeFactory;
import cz.bbn.cerberus.contactpersontype.persistence.ContactPersonTypeEntity;
import cz.bbn.cerberus.phoneprefix.factory.PhonePrefixFactory;
import cz.bbn.cerberus.phoneprefix.persistance.PhonePrefixEntity;

public class ContactPersonFactory {

    private ContactPersonFactory() {
    }

    public static ContactPersonDto fromEntity(ContactPersonEntity entity) {
        ContactPersonDto dto = new ContactPersonDto();
        dto.setId(entity.getId());
        dto.setFirstName(entity.getFirstName());
        dto.setLastName(entity.getLastName());
        if (entity.getPhonePrefixEntity() != null) {
            dto.setPhonePrefixDto(PhonePrefixFactory.getEntity(entity.getPhonePrefixEntity()));
        }
        dto.setPhone(entity.getPhone());

        if (entity.getPhonePrefix2Entity() != null) {
            dto.setPhonePrefix2Dto(PhonePrefixFactory.getEntity(entity.getPhonePrefix2Entity()));
        }
        dto.setPhone2(entity.getPhone2());

        dto.setDescription(entity.getDescription());
        dto.setEmail(entity.getEmail());
        dto.setOtherContacts(entity.getOtherContacts());
        if (entity.getContactPersonTypeEntity() != null) {
            ContactPersonTypeDto contactPersonTypeDto =
                    ContactPersonTypeFactory.fromEntity(entity.getContactPersonTypeEntity());
            dto.setContactPersonType(contactPersonTypeDto);
        }
        dto.setContactPersonPosition(entity.getContactPersonPosition());
        dto.setDeleted(entity.getDeleted());
        return dto;
    }

    public static void fillEntity(ContactPersonEntity entity, ContactPersonDto dto) {
        entity.setId(dto.getId());
        entity.setFirstName(dto.getFirstName());
        entity.setLastName(dto.getLastName());

        PhonePrefixEntity phonePrefixEntity = new PhonePrefixEntity();
        PhonePrefixFactory.fillEntity(phonePrefixEntity, dto.getPhonePrefixDto());
        entity.setPhonePrefixEntity(phonePrefixEntity);

        entity.setPhone(dto.getPhone());

        PhonePrefixEntity phonePrefix2Entity = new PhonePrefixEntity();
        PhonePrefixFactory.fillEntity(phonePrefix2Entity, dto.getPhonePrefix2Dto());
        entity.setPhonePrefix2Entity(phonePrefix2Entity);

        entity.setPhone2(dto.getPhone2());

        entity.setDescription(dto.getDescription());
        entity.setEmail(dto.getEmail());
        entity.setOtherContacts(dto.getOtherContacts());

        ContactPersonTypeEntity contactPersonTypeEntity = new ContactPersonTypeEntity();
        ContactPersonTypeFactory.fillEntity(contactPersonTypeEntity, dto.getContactPersonType());
        entity.setContactPersonTypeEntity(contactPersonTypeEntity);
        entity.setContactPersonPosition(dto.getContactPersonPosition());

        entity.setDeleted(false);
    }
}
