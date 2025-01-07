package cz.bbn.cerberus.technology.persistance.repository;

import cz.bbn.cerberus.technology.persistance.entity.TechnologyEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.List;


public interface TechnologyRepository
        extends JpaRepository<TechnologyEntity, String>, JpaSpecificationExecutor<TechnologyEntity> {

    @Query("select entity from TechnologyEntity entity where entity.deleted != true")
    List<TechnologyEntity> findAllNotDeleted();
}
