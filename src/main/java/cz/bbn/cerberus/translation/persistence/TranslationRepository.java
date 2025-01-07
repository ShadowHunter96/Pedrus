package cz.bbn.cerberus.translation.persistence;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Set;

public interface TranslationRepository extends JpaRepository<TranslationEntity, Long>,
        JpaSpecificationExecutor<TranslationEntity> {

    @Query(" select entity " +
            "from TranslationEntity entity " +
            "where entity.lang is not null " +
            "and entity.key is not null " +
            "and entity.value is not null")
    List<TranslationEntity> findAllTransl();

    @Query("select entity.lang from TranslationEntity entity " +
            "where entity.lang is not null and entity.key is not null and entity.value is not null")
    Set<String> findLangSet();

    @Query("select entity from TranslationEntity entity where entity.lang = :lang and entity.key = :key")
    Set<TranslationEntity> findByKeyAndLang(String key, String lang);

    @Query("select entity.key from TranslationEntity entity where entity.key is not null")
    Set<String> getKeySet();

    @Query("select distinct entity.lang from TranslationEntity entity")
    Set<String> getLangSet();
}
