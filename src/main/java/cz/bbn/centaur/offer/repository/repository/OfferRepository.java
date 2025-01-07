package cz.bbn.cerberus.offer.repository.repository;

import cz.bbn.cerberus.offer.repository.entity.OfferEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface OfferRepository extends JpaRepository<OfferEntity, String>, JpaSpecificationExecutor<OfferEntity> {

    @Query("select entity from OfferEntity entity where entity.deleted = false")
    List<OfferEntity> findAllNotDeleted();

    @Query("select entity.id from OfferEntity entity where entity.deleted = false")
    List<String> findAllId();

    OfferEntity findFirstByOrderByIdDesc();

    @Modifying
    @Query("update OfferEntity entity set entity.processedByUserEntity.id = :userId where entity.id = :id")
    void updateOwner(Long userId, String id);

    @Modifying
    @Query("update OfferEntity entity set entity.processedByUserEntity.id = :newOwnerId where entity.processedByUserEntity.id = :ownerId")
    void updateOwner(Long ownerId, Long newOwnerId);
}
