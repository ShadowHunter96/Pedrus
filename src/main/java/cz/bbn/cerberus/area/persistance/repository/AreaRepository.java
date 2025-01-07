package cz.bbn.cerberus.area.persistance.repository;

import cz.bbn.cerberus.area.persistance.entity.AreaEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface AreaRepository extends JpaRepository<AreaEntity, String>, JpaSpecificationExecutor<AreaEntity> {

    @Query("select entity from AreaEntity entity where entity.deleted != true")
    List<AreaEntity> findAllNotDeleted();
}
