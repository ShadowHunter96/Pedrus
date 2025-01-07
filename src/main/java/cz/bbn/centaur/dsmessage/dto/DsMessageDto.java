package cz.bbn.cerberus.dsmessage.dto;

import cz.bbn.cerberus.dsmessage.persistance.DsMessageType;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
public class DsMessageDto implements Serializable {

    private Long id;

    private String recipientId;
    private String senderId;
    private String senderName;
    private String senderAddress;
    private String subject;
    private String message;
    private int attachementSize;
    private LocalDateTime deliveryTime;
    private LocalDateTime acceptanceTime;
    private LocalDateTime createdInAppTime;
    private List<DsMessageAttachementDto> dsMessageAttachementDtoList;
    private Boolean viewed;
    private Boolean deleted;
    private DsMessageType type;
}
