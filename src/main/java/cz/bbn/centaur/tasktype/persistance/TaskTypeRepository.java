package cz.bbn.cerberus.tasktype.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface TaskTypeRepository extends JpaRepository<TaskTypeEntity, Long>,
        JpaSpecificationExecutor<TaskTypeEntity> {

    @Query("select entity from TaskTypeEntity entity where entity.archived = false")
    List<TaskTypeEntity> findAllNonArchived();
}
