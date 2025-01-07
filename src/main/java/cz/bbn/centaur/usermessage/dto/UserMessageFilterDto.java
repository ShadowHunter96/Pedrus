package cz.bbn.cerberus.usermessage.dto;


import cz.bbn.cerberus.usermessage.UserMessageObjectType;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class UserMessageFilterDto {

    private Long userId;
    private boolean orderByPriority;
    private UserMessageObjectType objectType;

    private int page;
    private int size;
}
