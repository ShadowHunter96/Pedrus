package cz.bbn.cerberus.asset.persistance.repository;

import cz.bbn.cerberus.asset.persistance.entity.AssetEntity;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface AssetRepository extends JpaRepository<AssetEntity, String> {

    @Query("select entity.id from AssetEntity entity where entity.assetPositionEntity.id = :id")
    List<String> getAssetIdByPositionId(String id);

    @Query("select entity.id from AssetEntity entity order by entity.id desc")
    String findLastId(PageRequest of);
}
