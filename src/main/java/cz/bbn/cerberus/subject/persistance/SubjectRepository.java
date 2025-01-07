package cz.bbn.cerberus.subject.persistance;

import cz.bbn.cerberus.commons.component.ui.domain.ItemEntity;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.suppliertype.persistance.SupplierTypeEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Set;

public interface SubjectRepository
        extends JpaRepository<SubjectEntity, String>, JpaSpecificationExecutor<SubjectEntity> {

    @Query("select new ItemEntity(entity.id, entity.name) " +
            "from SubjectEntity entity where deleted != true and entity.id in (:subjectIdSet)")
    List<ItemEntity> findAllAllowedItemList(Set<String> subjectIdSet);

    @Query("select entity " +
            "from SubjectEntity entity where deleted != true and entity.id in (:subjectIdSet)")
    List<SubjectEntity> findAllowedItemList(Set<String> subjectIdSet);

    @Query("select entity.id from SubjectEntity entity where entity.deleted = false")
    List<String> findAllId();

    @Query("select entity " +
            "from SubjectEntity entity " +
            "right join ContactPersonByObjectEntity objectEntity on entity.id = objectEntity.addedToObjectId " +
            "and objectEntity.objectType = 'CUSTOMER' " +
            "where entity.deleted = false and objectEntity.contactPerson.id = :contactPersonId")
    List<SubjectEntity> getCustomerEntityListByEntity(String contactPersonId);

    @Query("select entity " +
            "from SubjectEntity entity " +
            "right join SubjectByObjectEntity objectEntity on entity.id = objectEntity.id.subjectId " +
            "and objectEntity.id.objectType = :objectType " +
            "where entity.deleted = false and objectEntity.id.objectId = :objectId")
    Page<SubjectEntity> getSubjectPageByObject(String objectId, ObjectType objectType, Pageable pageable);

    @Query("select entity.id from SubjectEntity entity where entity.supplierType.id = :id")
    List<String> getIdListBySupplierTypeId(String id);

    @Modifying
    @Query("update SubjectEntity entity set entity.supplierType = :newValue where entity.supplierType = :oldValue")
    void changeSupplierType(SupplierTypeEntity newValue, SupplierTypeEntity oldValue);

    @Query("select entity.id from SubjectEntity entity where entity.ico = :ico")
    List<String> findIdByIco(String ico);

    @Query("select entity.id from SubjectEntity entity where entity.ico = :ico or entity.dic = :dic")
    List<String> findByIcoOrDic(String ico, String dic);

    @Query(value = "SELECT * FROM subject SUB WHERE SUB.local_subject = true AND SUB.deleted = false " +
            "ORDER BY SUB.last_update_from_ares LIMIT :numberOfSub", nativeQuery = true)
    List<SubjectEntity> getListFromAresUpdate(@Param("numberOfSub") int numberOfSub);

    @Query("select entity.name from SubjectEntity entity where entity.ownCompany = true")
    List<String> getOwnCompanyNameList();

    @Modifying
    @Query("update SubjectEntity entity set entity.userEntity.id = :userId where entity.id = :id")
    void updateOwner(Long userId, String id);

    @Modifying
    @Query("update SubjectEntity entity set entity.userEntity.id = :newOwnerId where entity.userEntity.id = :ownerId")
    void updateOwner(Long ownerId, Long newOwnerId);

}
