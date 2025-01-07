package cz.bbn.cerberus.virtualserver.dto;

public enum VirtualServerAction {
    CANCEL_REQUEST("Cancel request", "VS_CANCEL_REQUEST"),
    REQUEST_DELETING("Request deleting", "VS_REQUEST_DELETING"),
    CANCEL_DELETING("Cancel deleting", "VS_CANCEL_DELETING"),
    CREATE("Create", "VS_CREATE"),
    DELETE("Delete", "VS_DELETE"),
    REQUEST("Request", "VS_REQUEST"),
    DO_YOU_STILL_NEED_THIS_SERVER("Do you still need server {0}?", "VS_DO_YOU_STILL_NEED_THIS");

    private final String action;
    private final String notificationTitle;

    VirtualServerAction(String action, String notificationTitle) {
        this.action = action;
        this.notificationTitle = notificationTitle;
    }

    public String getAction() {
        return this.action;
    }

    public String getNotificationTitle() {
        return this.notificationTitle;
    }
}
