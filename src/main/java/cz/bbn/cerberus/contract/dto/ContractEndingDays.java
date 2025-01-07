package cz.bbn.cerberus.contract.dto;

public enum ContractEndingDays {

    DAYS_30(30), DAYS_60(60), DAYS_90(90);

    private final int days;

    ContractEndingDays(int days) {
        this.days = days;
    }

    public int getDays() {
        return days;
    }

    public static ContractEndingDays getByDays(Integer days) {
        for (ContractEndingDays endingDays : ContractEndingDays.values()) {
            if (endingDays.days == days) {
                return endingDays;
            }
        }
        return DAYS_30;
    }
}