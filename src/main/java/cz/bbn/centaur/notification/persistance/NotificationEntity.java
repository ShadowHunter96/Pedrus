package cz.bbn.cerberus.notification.persistance;

import cz.bbn.cerberus.notification.enums.NotificationPriority;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "notification_global", schema = "intranet")
@Getter
@Setter
public class NotificationEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String message;
    private String title;

    private String type;

    @Column(name = "user_id")
    private Long userId;

    private boolean sent;

    @Enumerated(EnumType.STRING)
    private NotificationPriority priority;
}
