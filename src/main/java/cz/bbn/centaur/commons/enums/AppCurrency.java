package cz.bbn.cerberus.commons.enums;

public enum AppCurrency {
    CZK, EUR, USD;

    public static AppCurrency valueOfOrCzk(String appCurrency) {
        for (AppCurrency currency : AppCurrency.values()) {
            if (currency.name().equals(appCurrency)) {
                return currency;
            }
        }
        return CZK;
    }
}
