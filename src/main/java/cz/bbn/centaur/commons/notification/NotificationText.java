package cz.bbn.cerberus.commons.notification;

/**
 * Zde se uchovavaji texty, ktere se vyskytuji casto v aplikaci a je potreba je mit na jednom miste
 */
public enum NotificationText {

    SAVED("Saved!"),
    DELETED("Deleted!"),
    CANNOT_BE_EMPTY("This field cannot be empty!");

    private final String text;

    NotificationText(String text) {
        this.text = text;
    }

    public String getText() {
        return text;
    }
}
