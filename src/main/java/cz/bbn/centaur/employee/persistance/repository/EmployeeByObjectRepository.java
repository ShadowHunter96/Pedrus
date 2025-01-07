package cz.bbn.cerberus.employee.persistance.repository;

import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.employee.persistance.entity.EmployeeByObjectEntity;
import cz.bbn.cerberus.employee.persistance.entity.EmployeeByObjectId;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface EmployeeByObjectRepository extends JpaRepository<EmployeeByObjectEntity, EmployeeByObjectId>,
        JpaSpecificationExecutor<EmployeeByObjectEntity> {

    @Query("select entity.id.employeeId " +
            "from EmployeeByObjectEntity entity " +
            "where entity.id.objectId = :objectId and entity.id.objectType = :objectType")
    List<String> getLinkedEmployeeIdList(String objectId, ObjectType objectType);

    @Query("select entity.id.objectId from EmployeeByObjectEntity entity where entity.id.employeeId = :employeeId " +
            "and entity.id.objectType in :objectTypeSet")
    List<String> getObjectIdByEmployeeAndType(String employeeId, List<ObjectType> objectTypeSet);
}
