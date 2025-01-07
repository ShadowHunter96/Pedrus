package cz.bbn.cerberus.assetposition.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface AssetPositionRepository
        extends JpaRepository<AssetPositionEntity, String>, JpaSpecificationExecutor<AssetPositionEntity> {
}
