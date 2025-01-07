package cz.bbn.cerberus.virtualserver.dto;

import cz.bbn.cerberus.translation.Transl;

public enum VirtualServerNotificationPeriod {

    MONTHLY("Monthly"),
    HALF_YEARLY("Half yearly"),
    YEARLY("Yearly"),
    NEVER("Never");

    private final String value;

    VirtualServerNotificationPeriod(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public static VirtualServerNotificationPeriod getFromName(String name) {
        for (VirtualServerNotificationPeriod period : VirtualServerNotificationPeriod.values()) {
            if (period.name().equals(name)) {
                return period;
            }
        }
        return NEVER;
    }

    public String getTranslatedValue() {
        return Transl.get(value);
    }
}
