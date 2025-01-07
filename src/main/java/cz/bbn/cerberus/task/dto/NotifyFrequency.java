package cz.bbn.cerberus.task.dto;

import cz.bbn.cerberus.translation.Transl;

public enum NotifyFrequency {

    NEVER("Never"), AT_BEGINNING("At the beginning"), WEEKLY("Weekly"), DAILY("Daily");

    private final String value;

    NotifyFrequency(String value) {
        this.value = value;
    }

    public String getTranslatedValue() {
        return Transl.get(value);
    }

    public static NotifyFrequency getFromNameOrDefault(String name) {
        for (NotifyFrequency notifyFrequency : NotifyFrequency.values()) {
            if (notifyFrequency.name().equals(name)) {
                return notifyFrequency;
            }
        }
        return NEVER;
    }
}
