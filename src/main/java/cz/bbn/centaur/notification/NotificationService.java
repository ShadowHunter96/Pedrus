package cz.bbn.cerberus.notification;

import cz.bbn.cerberus.notification.enums.NotificationPriority;
import cz.bbn.cerberus.notification.persistance.NotificationEntity;
import cz.bbn.cerberus.notification.persistance.NotificationRepository;
import cz.bbn.cerberus.task.dto.SendTask;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;

@Service
public class NotificationService {

    private final NotificationRepository notificationRepository;

    public NotificationService(NotificationRepository notificationRepository) {
        this.notificationRepository = notificationRepository;
    }

    @Transactional
    public void saveNotification(NotificationEntity notificationEntity) {
        notificationRepository.save(notificationEntity);
    }

    public void saveEmailLowNotification(String title, String message, Long userId) {
        saveEmailNotification(title, message, userId, NotificationPriority.LOW, SendTask.EMAIL);
    }

    public void saveEmailHighNotification(String title, String message, Long userId) {
        saveEmailNotification(title, message, userId, NotificationPriority.HIGH, SendTask.EMAIL);
    }

    public void saveAppNotificationLowNotification(String title, String message, Long userId) {
        saveEmailNotification(title, message, userId, NotificationPriority.LOW, SendTask.APP_NOTIFICATION);
    }

    public void saveAppNotificationHighNotification(String title, String message, Long userId) {
        saveEmailNotification(title, message, userId, NotificationPriority.HIGH, SendTask.APP_NOTIFICATION);
    }

    private void saveEmailNotification(String title, String message, Long userId, NotificationPriority priority, SendTask sendTask) {
        NotificationEntity notificationEntity = new NotificationEntity();
        notificationEntity.setSent(false);
        notificationEntity.setType(sendTask.name());
        notificationEntity.setTitle(title);
        notificationEntity.setMessage(message);
        notificationEntity.setUserId(userId);
        notificationEntity.setPriority(priority);
        saveNotification(notificationEntity);
    }
}
