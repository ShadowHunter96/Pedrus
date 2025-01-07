package cz.bbn.cerberus.assetposition.factory;


import cz.bbn.cerberus.assetposition.dto.AssetPositionDto;
import cz.bbn.cerberus.assetposition.persistance.AssetPositionEntity;

public class AssetPositionFactory {

    private AssetPositionFactory() {
    }

    public static AssetPositionDto fromEntity(AssetPositionEntity assetPositionEntity){
        AssetPositionDto assetPositionDto = new AssetPositionDto();
        assetPositionDto.setId(assetPositionEntity.getId());
        assetPositionDto.setName(assetPositionEntity.getName());
        assetPositionDto.setDescription(assetPositionEntity.getDescription());
        assetPositionDto.setLongitude(assetPositionEntity.getLongitude());
        assetPositionDto.setLatitude(assetPositionEntity.getLatitude());
        assetPositionDto.setDeleted(assetPositionEntity.getDeleted());
        return assetPositionDto;
    }

    public static void fillEntity(AssetPositionEntity entity, AssetPositionDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setDescription(dto.getDescription());
        entity.setLongitude(dto.getLongitude());
        entity.setLatitude(dto.getLatitude());
        entity.setDeleted(dto.getDeleted());
    }
}
