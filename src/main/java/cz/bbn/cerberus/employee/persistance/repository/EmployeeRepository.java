package cz.bbn.cerberus.employee.persistance.repository;

import cz.bbn.cerberus.employee.persistance.entity.EmployeeEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDate;
import java.util.List;
import java.util.Set;

public interface EmployeeRepository extends
        JpaRepository<EmployeeEntity, String>, JpaSpecificationExecutor<EmployeeEntity> {

    @Query("select entity from EmployeeEntity entity where entity.active = true and entity.deleted != true")
    List<EmployeeEntity> findValidList();

    @Query("select count(entity) from EmployeeEntity entity where entity.active = true " +
            "and entity.deleted != true and entity.startDate <= :end " +
            "and (entity.dismissDate >= :start or entity.dismissDate = null)")
    Long findEmployeeCountByPeriod(LocalDate start, LocalDate end);

    @Query("select entity.id from EmployeeEntity entity where entity.active = true " +
            "and entity.deleted != true and entity.startDate <= :end " +
            "and (entity.dismissDate >= :start or entity.dismissDate = null)")
    Set<String> findEmployeeIdSetByPeriod(LocalDate start, LocalDate end);
}
