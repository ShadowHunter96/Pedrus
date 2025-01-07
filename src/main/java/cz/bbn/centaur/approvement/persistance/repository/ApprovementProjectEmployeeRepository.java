package cz.bbn.cerberus.approvement.persistance.repository;

import cz.bbn.cerberus.approvement.persistance.entity.ApprovementProjectEmployeeEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.Set;

public interface ApprovementProjectEmployeeRepository extends JpaRepository<ApprovementProjectEmployeeEntity, Long> {

    @Query("select entity from ApprovementProjectEmployeeEntity entity where entity.id.approvementId = :id")
    Set<ApprovementProjectEmployeeEntity> findByApprovementId(Long id);
}
