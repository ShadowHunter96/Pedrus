package cz.bbn.cerberus.asset.dto;

import cz.bbn.cerberus.translation.Transl;

public enum AssetStateEnum {

    IN_STOCK,
    ALLOCATED,
    IN_REPAIR;


    public static String translate(AssetStateEnum assetStateEnum) {
        if (assetStateEnum != null) {
            return Transl.get(assetStateEnum.name().toLowerCase());
        }
        return "";
    }
}
