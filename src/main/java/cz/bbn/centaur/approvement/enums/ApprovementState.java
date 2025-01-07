package cz.bbn.cerberus.approvement.enums;


public enum ApprovementState {

    WAIT_FOR_LM_MANAGER("validation-orange"), WAITING_FOR_SUPERIOR("validation-orange"),
    COMPLETELY_APPROVED("validation-green"), DENIED("validation-red"),
    RETURNED_FOR_COMPLETION("validation-orange"), CANCELED("validation-red");

    private final String colorClass;

    ApprovementState(String colorClass) {
        this.colorClass = colorClass;
    }

    public String getColorClass() {
        return colorClass;
    }
}
