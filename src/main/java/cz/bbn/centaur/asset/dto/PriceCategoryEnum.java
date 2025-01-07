package cz.bbn.cerberus.asset.dto;

public enum PriceCategoryEnum {

    FIRST("1-10k"),
    SECOND("10k-40k"),
    THIRD("40K- 250k"),
    FOURTH("250K- 500K"),
    FIFTH("500K - 1M"),
    SIXTH("1M+");

    private final String textValue;

    PriceCategoryEnum(String textValue) {
        this.textValue = textValue;
    }

    public static PriceCategoryEnum getCategory(Double price) {
        if (price < 10000) {
            return FIRST;
        }
        if (price < 40000) {
            return SECOND;
        }
        if (price < 250000) {
            return THIRD;
        }
        if (price < 500000) {
            return FOURTH;
        }
        if (price < 1000000) {
            return FIFTH;
        }
        return SIXTH;
    }

    public String getTextValue() {
        return textValue;
    }
}
