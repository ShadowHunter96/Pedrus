package cz.bbn.cerberus.opportunity.persistance.repository;

import cz.bbn.cerberus.opportunity.persistance.entity.OpportunityEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface OpportunityRepository extends JpaRepository<OpportunityEntity, String> {

    @Query("select entity.id from OpportunityEntity entity where entity.deleted = false")
    List<String> findAllId();

    OpportunityEntity findFirstByOrderByIdDesc();

    @Modifying
    @Query("update OpportunityEntity entity set entity.userEntity.id = :userId where entity.id = :id")
    void updateOwner(Long userId, String id);

    @Modifying
    @Query("update OpportunityEntity entity set entity.userEntity.id = :newOwnerId where entity.userEntity.id = :ownerId")
    void updateOwner(Long ownerId, Long newOwnerId);
}
