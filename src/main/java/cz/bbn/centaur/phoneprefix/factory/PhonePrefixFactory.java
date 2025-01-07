package cz.bbn.cerberus.phoneprefix.factory;

import cz.bbn.cerberus.phoneprefix.dto.PhonePrefixDto;
import cz.bbn.cerberus.phoneprefix.persistance.PhonePrefixEntity;

public class PhonePrefixFactory {

    private PhonePrefixFactory() {
    }

    public static PhonePrefixDto getEntity(PhonePrefixEntity entity){
        PhonePrefixDto phonePrefixDto = new PhonePrefixDto();
        phonePrefixDto.setCountryCode(entity.getCountryCode());
        phonePrefixDto.setPhonePrefix(entity.getPhonePrefix());
        return phonePrefixDto;
    }

    public static void fillEntity(PhonePrefixEntity entity, PhonePrefixDto phonePrefixDto){
        entity.setCountryCode(phonePrefixDto.getCountryCode());
        entity.setPhonePrefix(phonePrefixDto.getPhonePrefix());
    }
}
