package cz.bbn.cerberus.usermessage.persistance;

import cz.bbn.cerberus.usermessage.MessageType;
import cz.bbn.cerberus.usermessage.UserMessageObjectType;
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
import java.time.LocalDateTime;

@Entity
@Table(name = "notification_app", schema = "intranet")
@Getter
@Setter
public class UserMessageEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String message;
    private Boolean priority;

    @Enumerated(EnumType.STRING)
    private MessageType type;

    @Column(name = "due_date")
    private LocalDateTime dueDate;

    @Column(name = "object_id")
    private String objectId;

    @Column(name = "object_type")
    @Enumerated(EnumType.STRING)
    private UserMessageObjectType objectType;

    private Boolean viewed;

    @Column(name = "user_id")
    private Long userId;

}
