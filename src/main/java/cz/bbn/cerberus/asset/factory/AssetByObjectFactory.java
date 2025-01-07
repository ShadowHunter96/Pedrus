package cz.bbn.cerberus.asset.factory;

import cz.bbn.cerberus.asset.dto.AssetByObjectDto;
import cz.bbn.cerberus.asset.persistance.entity.AssetByObjectEntity;

public class AssetByObjectFactory {

    private AssetByObjectFactory() {
    }

    public static AssetByObjectDto fromEntity(AssetByObjectEntity entity) {
        AssetByObjectDto dto = new AssetByObjectDto();
        dto.setId(entity.getId());
        if (entity.getAsset() != null) {
            dto.setId(entity.getId());
            dto.setAssetId(entity.getAsset().getId());
            dto.setName(entity.getAsset().getName());
            dto.setSerialNumber(entity.getAsset().getSerialNumber());
            dto.setType(entity.getAsset().getType());
            dto.setBuyDate(entity.getAsset().getBuyDate());
            dto.setPrice(entity.getAsset().getPrice());
            dto.setResponsiblePerson(entity.getAsset().getResponsiblePerson());
        }
        return dto;
    }
}
