package cz.bbn.cerberus.dph.persistance.repository;

import cz.bbn.cerberus.dph.persistance.entity.DphEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface DphRepository extends JpaRepository<DphEntity, String>, JpaSpecificationExecutor<DphEntity> {

    @Query("select entity from DphEntity entity where entity.allowed = true")
    List<DphEntity> findAllAllowed();

    @Query("select entity from DphEntity entity where entity.id != :id and defaultValue = true")
    List<DphEntity> defaultValueTrueList(String id);

    @Query("select entity from DphEntity entity where entity.allowed = true and entity.defaultValue = true")
    List<DphEntity> getAllowedDefaultList();

}
