package cz.bbn.cerberus.phoneprefix;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.phoneprefix.dto.PhonePrefixDto;
import cz.bbn.cerberus.phoneprefix.factory.PhonePrefixFactory;
import cz.bbn.cerberus.phoneprefix.persistance.PhonePrefixEntity;
import cz.bbn.cerberus.phoneprefix.persistance.PhonePrefixRepository;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class PhonePrefixService {

    private final PhonePrefixRepository phonePrefixRepository;

    public PhonePrefixService(PhonePrefixRepository phonePrefixRepository) {
        this.phonePrefixRepository = phonePrefixRepository;
    }

    public List<PhonePrefixDto> findAllPhonePrefixDtoList(){
        List<PhonePrefixEntity> list = phonePrefixRepository.findAll();
        return ConvertEntities.fromEntities(list, PhonePrefixFactory::getEntity);
    }
}
