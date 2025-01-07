package cz.bbn.cerberus.translation;

import cz.bbn.cerberus.translation.persistence.TranslationEntity;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.UserValues;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.text.MessageFormat;
import java.util.List;
import java.util.Map;

@Component
@Slf4j
public class Transl {

    public static final String DEFAULT_LANG = "cs";

    private static Map<String, List<TranslationEntity>> translations;
    private static TranslationService translationService;
    private static List<String> translationKeyList;
    private static UserValues userValues;

    public Transl(TranslationService translationService, UserValues userValues) {
        Transl.translationService = translationService;
        initTranslations();
        this.userValues = userValues;
    }

    public Map<String, List<TranslationEntity>> getTranslations() {
        return translations;
    }

    public static String getByLang(String key, String language) {
        if (userValues.isTurnOffTranslations()) {
            return key;
        }

        if (key == null) {
            return "???";
        }

        String translation = getTranslation(key, language);

        if (translation == null && !language.equals(DEFAULT_LANG)) {
            translation = getTranslation(key, DEFAULT_LANG);
        }
        if (translation == null) {
            translation = "? " + key;
        }
        return translation;
    }

    public static String get(String key) {
        String language = UserService.getApplicationTranslation().name().toLowerCase();
        return getByLang(key, language);
    }

    public static String get(String key, String... params) {
        String translation = get(key);

        if (key.contains("{0}")) {
            return MessageFormat.format(translation, params);
        } else {
            return String.format(translation, params);
        }
    }

    private static String getTranslation(String key, String language) {
        String translation = null;
        if (key == null) {
            return null;
        }
        for (TranslationEntity item : translations.get(language)) {
            if (key.equalsIgnoreCase(item.getKey())) {
                translation = item.getValue();
            }
        }

        if ("".equals(key)) {
            translation = "";
        }

        if (translationKeyList != null && !translationKeyList.contains(key) && !"".equals(key)) {
            translationService.addTranslation(key);
        }
        return translation;
    }

    public static void setTranslations(Map<String, List<TranslationEntity>> translations) {
        Transl.translations = translations;
    }

    public synchronized void initTranslations() {
        translations = translationService.getTranslations();
        translationKeyList = translationService.getTranslationKeyList();
        log.info("translations reloaded");
    }
}
