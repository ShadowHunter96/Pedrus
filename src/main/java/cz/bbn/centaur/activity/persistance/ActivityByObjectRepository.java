package cz.bbn.cerberus.activity.persistance;

import cz.bbn.cerberus.commons.enums.ObjectType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface ActivityByObjectRepository extends JpaRepository<ActivityByObjectEntity, ActivityByObjectId>,
        JpaSpecificationExecutor<ActivityByObjectEntity> {

    @Query("select entity.id.activityId " +
            "from ActivityByObjectEntity entity " +
            "where entity.id.objectId = :objectId and entity.id.objectType = :objectType")
    List<Long> getLinkedActivityIdList(String objectId, ObjectType objectType);

    @Query("select entity from ActivityByObjectEntity entity where entity.enumerationEntity.deleted = false " +
            "and entity.enumerationEntity.allowed = true")
    List<ActivityByObjectEntity> findAllAllowed();
}
