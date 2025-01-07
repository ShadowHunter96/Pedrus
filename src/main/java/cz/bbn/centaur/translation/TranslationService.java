package cz.bbn.cerberus.translation;

import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.translation.dto.TranslationDto;
import cz.bbn.cerberus.translation.dto.TranslationFilterDto;
import cz.bbn.cerberus.translation.factory.TranslationFactory;
import cz.bbn.cerberus.translation.persistence.TranslationDao;
import cz.bbn.cerberus.translation.persistence.TranslationEntity;
import cz.bbn.cerberus.translation.persistence.TranslationRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpServerErrorException;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@Slf4j
public class TranslationService {

    private final TranslationRepository translationRepository;
    private final TranslationDao translationDao;

    public TranslationService(TranslationRepository translationRepository, TranslationDao translationDao) {
        this.translationRepository = translationRepository;
        this.translationDao = translationDao;
    }

    public Map<String, List<TranslationEntity>> getTranslations() {
        List<TranslationEntity> list = new ArrayList<>();
        try {
            list.addAll(translationRepository.findAllTransl());
        } catch (HttpServerErrorException ex) {
            log.error("HttpServerErrorException", ex);
        }
        return list
                .stream()
                .collect(Collectors.groupingBy(TranslationEntity::getLang));
    }

    public List<String> getTranslationKeyList() {
        return translationRepository.getKeySet().stream().toList();
    }

    public void addTranslation(String key) {
        Set<String> langSet = new HashSet<>();
        langSet.add("cs");
        langSet.add("en");
        for (String lang : langSet) {
            if (translationRepository.findByKeyAndLang(key, lang).isEmpty()) {
                TranslationEntity entity = new TranslationEntity();
                entity.setKey(key);
                entity.setLang(lang);
                translationRepository.save(entity);
            }
        }
    }

    public void deleteTranslation(Long id) throws SystemException {
        if (translationRepository.existsById(id)) {
            translationRepository.deleteById(id);
        } else {
            throw new SystemException(ErrorCode.TRANSLATION_DOES_NOT_EXISTS, id);
        }
    }

    public Page<TranslationDto> getTranslationPage(TranslationFilterDto translationFilterDto) {
        return translationDao.findTranslationPage(translationFilterDto);
    }

    public Set<String> getLangSet() {
        return translationRepository.getLangSet();
    }

    public TranslationDto getTranslationDto(String lang, String key) {
        Set<TranslationEntity> entitySet = translationRepository.findByKeyAndLang(key, lang);
        if (!entitySet.isEmpty()) {
            TranslationDto toReturn = new TranslationDto();
            for (TranslationEntity entity : entitySet) {
                toReturn = TranslationFactory.fromEntity(entity);
                break;
            }
            return toReturn;
        }
        TranslationDto dto = new TranslationDto();
        dto.setLang(lang);
        dto.setKey(key);
        return dto;
    }

    public void save(TranslationDto dto) {
        TranslationEntity entity = new TranslationEntity();
        TranslationFactory.fillEntity(entity, dto);
        translationRepository.save(entity);
    }
}
