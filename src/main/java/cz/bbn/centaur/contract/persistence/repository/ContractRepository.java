package cz.bbn.cerberus.contract.persistence.repository;

import cz.bbn.cerberus.commons.component.ui.domain.ItemEntity;
import cz.bbn.cerberus.contract.persistence.entity.ContractEntity;
import cz.bbn.cerberus.contracttype.persistence.ContractTypeEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Set;

public interface ContractRepository
        extends JpaRepository<ContractEntity, String>, JpaSpecificationExecutor<ContractEntity> {

    @Query("select entity.id from ContractEntity entity where entity.deleted = false")
    List<String> findAllId();

    @Query("select new ItemEntity(entity.id, entity.name) " +
            "from ContractEntity entity where deleted != true and entity.id in (:contractIdSet)")
    List<ItemEntity> findAllowedItemList(Set<String> contractIdSet);

    @Query("select entity " +
            "from ContractEntity entity where deleted != true and entity.id in (:contractIdSet)")
    List<ContractEntity> findAllAllowedItemList(Set<String> contractIdSet);

    @Query(" select entity " +
            "from ContractEntity entity " +
            "where entity.subject.id = :subjectId")
    List<ContractEntity> getContractListBySubject(String subjectId);

    @Query("select entity.sequence from ContractEntity entity " +
            "where entity.sequence != null " +
            "order by entity.sequence desc")
    List<Integer> getSequenceList();

    @Query("select entity.subsequence from ContractEntity entity " +
            "where entity.connectedContract.id = :id and entity.subsequence != null order by entity.subsequence desc")
    List<Integer> getSubSequence(String id);

    @Modifying
    @Query(" update ContractEntity entity " +
            "set entity.type = :newType " +
            "where entity.type = :oldType")
    void changeContractType(ContractTypeEntity oldType, ContractTypeEntity newType);

    @Query(" select entity.id " +
            "from ContractEntity entity " +
            "where entity.type.id = :id")
    List<String> getUsedContractTypeList(String id);

    @Modifying
    @Query("update ContractEntity entity set entity.userEntity.id = :userId where entity.id = :id")
    void updateOwner(Long userId, String id);

    @Modifying
    @Query("update ContractEntity entity set entity.userEntity.id = :newOwnerId where entity.userEntity.id = :ownerId")
    void updateOwner(Long ownerId, Long newOwnerId);
}
