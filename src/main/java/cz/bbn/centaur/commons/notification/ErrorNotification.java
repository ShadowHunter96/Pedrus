package cz.bbn.cerberus.commons.notification;


import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.notification.NotificationVariant;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ErrorNotification {

    private ErrorNotification() {
    }

    public static Notification show(String messageForUser, Exception ex, AppEnv appEnv) {
        return showPrivate(messageForUser.concat(" - ").concat(ex.getMessage()), appEnv);
    }

    public static Notification show(Exception ex, AppEnv appEnv) {
        return showPrivate(ex.getMessage(), appEnv);
    }

    public static Notification show(String text, AppEnv appEnv) {
        return showPrivate(text, appEnv);
    }

    public static Notification show(String text, AppEnv appEnv, String... params) {
        String formattedText = params == null ? text : String.format(text, params);
        return showPrivate(formattedText, appEnv);
    }

    private static Notification showPrivate(String text, AppEnv appEnv) {
        AppNotification notification = new AppNotification(text, VaadinIcon.CLOSE_CIRCLE);
        notification.addThemeVariants(NotificationVariant.LUMO_ERROR);
        notification.setPosition(Notification.Position.TOP_CENTER);
        notification.setNotificationDuration(appEnv.getErrorDuration());
        notification.open();
        return notification;
    }

}
