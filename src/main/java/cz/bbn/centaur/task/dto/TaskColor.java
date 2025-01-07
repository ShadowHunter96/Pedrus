package cz.bbn.cerberus.task.dto;

import cz.bbn.cerberus.translation.Transl;

public enum TaskColor {
    NONE("None", ""),
    GREEN("Green", "green"),
    RED("red", "red"),
    YELLOW("Yellow", "yellow"),
    BLUE("Blue", "blue");

    private final String value;
    private final String color;

    TaskColor(String value, String color) {
        this.value = value;
        this.color = color;
    }

    public String getTranslatedValue() {
        return Transl.get(value);
    }

    public String getColor() {
        return color;
    }

    public static TaskColor getFromNameOrDefault(String name) {
        for (TaskColor taskColor : TaskColor.values()) {
            if (taskColor.name().equals(name)) {
                return taskColor;
            }
        }
        return NONE;
    }
}
