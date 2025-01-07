package cz.bbn.cerberus.usermessage.dto;

import cz.bbn.cerberus.usermessage.UserMessageObjectType;
import cz.bbn.cerberus.usermessage.MessageType;
import lombok.Getter;
import lombok.Setter;


import java.time.LocalDateTime;

@Getter
@Setter
public class UserMessageDto {

    private Long id;

    private String message;
    private Boolean priority;
    private MessageType type;
    private LocalDateTime dueDate;
    private Long userId;
    private String objectId;
    private UserMessageObjectType objectType;
    private Boolean viewed;
}
