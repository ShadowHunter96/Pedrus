package cz.bbn.cerberus.taskschedule.dto;

import cz.bbn.cerberus.translation.Transl;

public enum TaskScheduleFrequency {

    WEEKLY("Weekly"),
    MONTHLY("Monthly"),
    QUARTERLY("Quarterly"),
    HALF_YEARLY("Half yearly"),
    YEARLY("Yearly");

    private final String value;

    TaskScheduleFrequency(String value) {
        this.value = value;
    }

    public String getTranslatedValue() {
        return Transl.get(value);
    }

    public static TaskScheduleFrequency getFromNameOrNull(String name) {
        for (TaskScheduleFrequency frequency : TaskScheduleFrequency.values()) {
            if (frequency.name().equals(name)) {
                return frequency;
            }
        }
        return null;
    }
}
