package cz.bbn.cerberus.contactperson.persistance.repository;

import cz.bbn.cerberus.contactperson.persistance.entity.ContactPersonByObjectEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Set;

public interface ContactPersonByObjectRepository extends
        JpaRepository<ContactPersonByObjectEntity, Long>, JpaSpecificationExecutor<ContactPersonByObjectEntity> {

    @Modifying
    @Query("delete from ContactPersonByObjectEntity entity " +
            "where entity.contactPerson.id = :id and " +
            "entity.objectType = :objectType and " +
            "entity.addedToObjectId = :addedObjectId")
    void delete(String id, String objectType, String addedObjectId);

    @Query("select entity.contactPerson.id from ContactPersonByObjectEntity entity " +
            "where entity.objectType like :objectType and entity.addedToObjectId in :objectIdSet " +
            "and entity.contactPerson.deleted = false")
    Set<String> findIdByObjectTypeAndObjectIdSet(String objectType, Set<String> objectIdSet);

    @Query("select entity.addedToObjectId from ContactPersonByObjectEntity entity " +
            "where entity.objectType like :objectType and entity.contactPerson.id like :id " +
            "and entity.contactPerson.deleted = false")
    List<String> findObjectIdSetByIdAndObjectType(String objectType, String id);

    @Query("select entity.addedToObjectId from ContactPersonByObjectEntity entity " +
            "where entity.objectType like :objectType " +
            "and entity.contactPerson.deleted = false")
    List<String> findObjectIdByObjectType(String objectType);

    List<ContactPersonByObjectEntity> findByObjectTypeAndAddedToObjectId(String objectType, String addedToObjectId);

    @Query("select entity.contactPerson.id from ContactPersonByObjectEntity entity " +
            "where entity.objectType like :objectType and entity.addedToObjectId like :id")
    Set<String> findIdSetByObjectTypeAddedObjectId(String objectType, String id);

    @Modifying
    @Query("delete from ContactPersonByObjectEntity entity where entity.contactPerson.id = :id")
    void deleteByContactPersonId(String id);
}
