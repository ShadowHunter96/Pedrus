package cz.bbn.cerberus.workreport.persistance.repository;

import cz.bbn.cerberus.workreport.persistance.entity.WorkReportClosedEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface WorkReportClosedRepository extends JpaRepository<WorkReportClosedEntity, Long> {

    @Query("select entity from WorkReportClosedEntity entity where entity.id.employeeId = :employeeId")
    List<WorkReportClosedEntity> findByEmployee(String employeeId);
}
