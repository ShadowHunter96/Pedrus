package cz.bbn.cerberus.commons.notification;

import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;

public class WarningNotification {

    private WarningNotification() {
    }

    public static Notification show(NotificationText notificationText, AppEnv appEnv) {
        return show(notificationText.getText(), appEnv);
    }

    public static Notification show(String text, AppEnv appEnv) {
        AppNotification notification = new AppNotification(text, VaadinIcon.WARNING);
        notification.setThemeName("warning");
        notification.setPosition(Notification.Position.TOP_CENTER);
        notification.setNotificationDuration(appEnv.getWarningDuration());
        notification.open();
        return notification;
    }

}
