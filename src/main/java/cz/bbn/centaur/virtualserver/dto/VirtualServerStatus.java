package cz.bbn.cerberus.virtualserver.dto;

import java.util.ArrayList;
import java.util.List;

public enum VirtualServerStatus {

    RUNNING("Running", "Green"),
    CREATING("Creating", "Blue"),
    DELETING("Deleting", "Red"),
    DELETED("Deleted", "Red"),
    NON_EXISTENT("", "");

    private final String value;
    private final String color;

    VirtualServerStatus(String value, String color) {
        this.value = value;
        this.color = color;
    }

    public static List<VirtualServerStatus> nonDeletedValues() {
        List<VirtualServerStatus> statusList = new ArrayList<>();
        for (VirtualServerStatus status : VirtualServerStatus.values()) {
            if (status != DELETED && status != NON_EXISTENT) {
                statusList.add(status);
            }
        }
        return statusList;
    }

    public String getValue() {
        return value;
    }

    public String getColor() {
        return color;
    }

    public static VirtualServerStatus getFromNameOrNonExistent(String name) {
        for (VirtualServerStatus status : VirtualServerStatus.values()) {
            if (status.name().equals(name)) {
                return status;
            }
        }
        return NON_EXISTENT;
    }
}
