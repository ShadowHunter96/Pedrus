package cz.bbn.cerberus.asset.persistance.repository;

import cz.bbn.cerberus.asset.persistance.entity.AssetByObjectEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.Set;

public interface AssetByObjectRepository extends
        JpaRepository<AssetByObjectEntity, Long>, JpaSpecificationExecutor<AssetByObjectEntity> {

    @Query("select entity.asset.id from AssetByObjectEntity entity where entity.addedToObjectId = :id")
    Set<String> getExistingIdConnection(String id);
}
