package cz.bbn.cerberus.commons.notification;

import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.notification.NotificationVariant;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;

public class SuccessNotification {

    private SuccessNotification() {
    }

    public static void showSavingSuccess(AppEnv appEnv) {
        show(NotificationText.SAVED, appEnv);
    }

    public static void showDeleteSuccess(AppEnv appEnv) {
        show(NotificationText.DELETED, appEnv);
    }

    public static Notification show(NotificationText notificationText, AppEnv appEnv) {
        return show(notificationText.getText(), appEnv);
    }

    public static Notification show(String text, AppEnv appEnv) {
        AppNotification notification = new AppNotification(text, VaadinIcon.CHECK_CIRCLE);
        notification.addThemeVariants(NotificationVariant.LUMO_SUCCESS);
        notification.setPosition(Notification.Position.TOP_CENTER);
        notification.setNotificationDuration(appEnv.getSuccessDuration());
        notification.open();
        return notification;
    }
}
