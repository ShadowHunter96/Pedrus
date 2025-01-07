package cz.bbn.cerberus.approvement.enums;

import com.vaadin.flow.component.icon.VaadinIcon;

public enum ApprovementType {

    HOLIDAY(VaadinIcon.FLIGHT_TAKEOFF,  true), HOME_OFFICE(VaadinIcon.HEADSET, false),
    ILL(VaadinIcon.SPECIALIST,  false), BUSSINES_TRIP(VaadinIcon.CAR,  false),
    UNPAID_LEAVE(VaadinIcon.MONEY_DEPOSIT,  true), PAID_LEAVE(VaadinIcon.MONEY_WITHDRAW, true);

    private final VaadinIcon icon;
    private final boolean employeeMandatory;

    ApprovementType(VaadinIcon icon, boolean employeeMandatory) {
        this.icon = icon;
        this.employeeMandatory = employeeMandatory;
    }

    public VaadinIcon getIcon() {
        return icon;
    }

    public boolean isEmployeeMandatory() {
        return employeeMandatory;
    }
}
