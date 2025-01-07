package cz.bbn.cerberus.dsmessage.persistance.entity;

import cz.bbn.cerberus.dsmessage.persistance.DsMessageType;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Id;
import javax.persistence.Table;
import java.time.LocalDateTime;

@Entity
@Table(name = "ds_message", schema = "backoffice")
@Getter
@Setter
public class DsMessageSimpleEntity {

    @Id
    private Long id;

    @Column(name = "recipient_ds_id")
    private String recipientId;

    @Column(name = "sender_name")
    private String senderName;

    @Column(name = "sender_address")
    private String senderAddress;

    private String subject;

    @Column(name = "attachement_size")
    private int attachementSize;

    @Column(name = "delivery_time")
    private LocalDateTime deliveryTime;

    @Column(name = "created_in_app_time")
    private LocalDateTime createdInAppTime;

    private Boolean viewed;
    private Boolean deleted;

    @Enumerated(EnumType.STRING)
    @Column(name = "message_type")
    private DsMessageType type;
}
