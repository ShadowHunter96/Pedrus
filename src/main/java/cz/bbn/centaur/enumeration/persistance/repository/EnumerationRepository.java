package cz.bbn.cerberus.enumeration.persistance.repository;

import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface EnumerationRepository
        extends JpaRepository<EnumerationEntity, Long>, JpaSpecificationExecutor<EnumerationEntity> {

    List<EnumerationEntity> findByEnumerationTypeEntityId(String enumeratorTypeId);

    @Query("select entity from EnumerationEntity entity " +
            "where entity.id != :id and value = 'true' " +
            "and entity.enumerationTypeEntity.id = :enumerationTypeId " +
            "and allowed = true and deleted = false")
    List<EnumerationEntity> getByDefaultValueTrueList(Long id, String enumerationTypeId);

    @Query("select entity from EnumerationEntity entity " +
            "where entity.enumerationTypeEntity.id = :enumerationTypeId " +
            "and allowed = true and deleted = false")
    List<EnumerationEntity> getAllowedNotDeletedByType(String enumerationTypeId);
}
