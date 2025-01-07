package cz.bbn.cerberus.dsmessage.dto;

import cz.bbn.cerberus.dsmessage.persistance.DsMessageType;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
public class DsMessageFilterDto {

    private LocalDateTime deliveryTimeFrom;
    private LocalDateTime deliveryTimeTo;
    private LocalDateTime createdInAppFrom;
    private LocalDateTime createdInAppTo;
    private String recipientId;
    private String senderName;
    private DsMessageType type;
    private boolean viewed;

    private boolean showDeleted;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
