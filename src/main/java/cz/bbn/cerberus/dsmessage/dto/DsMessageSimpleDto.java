package cz.bbn.cerberus.dsmessage.dto;

import cz.bbn.cerberus.dsmessage.persistance.DsMessageType;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.time.LocalDateTime;

@Getter
@Setter
public class DsMessageSimpleDto implements Serializable {

    private Long id;
    private String recipientId;
    private String senderName;
    private String subject;
    private int attachementSize;
    private LocalDateTime deliveryTime;
    private LocalDateTime createdInAppTime;
    private Boolean viewed;
    private Boolean deleted;
    private DsMessageType type;
}
