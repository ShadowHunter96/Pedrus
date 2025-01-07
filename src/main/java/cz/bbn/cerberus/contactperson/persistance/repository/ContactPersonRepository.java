package cz.bbn.cerberus.contactperson.persistance.repository;

import cz.bbn.cerberus.commons.component.ui.domain.ItemEntity;
import cz.bbn.cerberus.contactperson.persistance.entity.ContactPersonEntity;
import cz.bbn.cerberus.contactpersontype.persistence.ContactPersonTypeEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Set;

public interface ContactPersonRepository extends JpaRepository<ContactPersonEntity, String>,
        JpaSpecificationExecutor<ContactPersonEntity> {

    @Query(" select entity.id " +
            "from ContactPersonEntity entity " +
            "where entity.contactPersonTypeEntity.id = :id")
    List<String> getUsedContactPersonTypeList(String id);

    @Modifying
    @Query(" update ContactPersonEntity entity " +
            "set entity.contactPersonTypeEntity = :newType " +
            "where entity.contactPersonTypeEntity = :oldType")
    void changeContactPersonType(ContactPersonTypeEntity oldType, ContactPersonTypeEntity newType);

    @Query("select new ItemEntity(entity.id, CONCAT(entity.firstName, ' ', entity.lastName)) " +
            "from ContactPersonEntity entity where deleted != true and entity.id in :contactPersonIdSet ")
    List<ItemEntity> findAllAllowedItemList(Set<String> contactPersonIdSet);

    @Query("select entity.id from ContactPersonEntity entity " +
            "where entity.deleted = false and entity.id not in (" +
            "select entityByObject.contactPerson.id from ContactPersonByObjectEntity entityByObject " +
            "where entityByObject.objectType like :objectType and entityByObject.addedToObjectId like :objectId)" +
            "and entity.id in (:contactPersonSet)")
    List<String> findNotUsedByObjectByUser(String objectType, String objectId, Set<String> contactPersonSet);

    @Query("select entity.id from ContactPersonEntity entity where entity.deleted = false")
    List<String> findAllIdList();

    @Query("select count(entity) from ContactPersonEntity entity where entity.id like concat(concat('%', :id), '%')")
    int findCountContainId(String id);
}
