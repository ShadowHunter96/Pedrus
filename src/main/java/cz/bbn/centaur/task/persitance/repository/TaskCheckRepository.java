package cz.bbn.cerberus.task.persitance.repository;

import cz.bbn.cerberus.task.persitance.entity.TaskCheckEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface TaskCheckRepository extends JpaRepository<TaskCheckEntity, Long> {

    @Query("select entity from TaskCheckEntity entity where entity.entityId = :id and entity.entityType = :entityType")
    List<TaskCheckEntity> findListByIdAndType(Long id, String entityType);
}
