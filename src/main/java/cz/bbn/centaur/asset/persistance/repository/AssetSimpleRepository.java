package cz.bbn.cerberus.asset.persistance.repository;

import cz.bbn.cerberus.asset.persistance.entity.AssetSimpleEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface AssetSimpleRepository
        extends JpaRepository<AssetSimpleEntity, String>, JpaSpecificationExecutor<AssetSimpleEntity> {
}
