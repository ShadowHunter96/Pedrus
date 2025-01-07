package cz.bbn.cerberus.task.dto;

import com.vaadin.flow.component.icon.VaadinIcon;
import cz.bbn.cerberus.translation.Transl;

public enum TaskState {

    NEW("New task", VaadinIcon.FLAG_CHECKERED),
    ASSIGNED("Assigned task", VaadinIcon.FLAG_O),
    IN_PROGRESS("Task in progress", VaadinIcon.FLAG),
    RESOLVED("Resolved task", VaadinIcon.SMILEY_O),
    REJECTED("Rejected task", VaadinIcon.FROWN_O),
    CANCELLED("Cancelled task", VaadinIcon.FLASH);

    private final String value;
    private final VaadinIcon iconName;

    TaskState(String value, VaadinIcon iconName) {
        this.value = value;
        this.iconName = iconName;
    }

    public String getTranslatedValue() {
        return Transl.get(value);
    }

    public String getValue() {
        return value;
    }

    public VaadinIcon getIconName() {
        return iconName;
    }

    public static TaskState getFromNameOrDefault(String name) {
        for (TaskState state : TaskState.values()) {
            if (state.name().equals(name)) {
                return state;
            }
        }
        return NEW;
    }
}
