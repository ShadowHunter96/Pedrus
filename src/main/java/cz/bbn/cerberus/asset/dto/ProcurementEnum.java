package cz.bbn.cerberus.asset.dto;

import cz.bbn.cerberus.translation.Transl;

public enum ProcurementEnum {

    PURCHASE,
    HOMEMADE,
    SUBSIDY,
    GIFT,
    INVESTMENT_IN_BUSINESS,
    OTHER;

    public static String translate(ProcurementEnum procurementEnum) {
        if (procurementEnum != null) {
            return Transl.get(procurementEnum.name().toLowerCase());
        }
        return null;
    }
}
