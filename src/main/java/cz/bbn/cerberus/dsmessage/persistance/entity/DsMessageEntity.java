package cz.bbn.cerberus.dsmessage.persistance.entity;

import cz.bbn.cerberus.dsmessage.persistance.DsMessageType;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "ds_message", schema = "backoffice")
@Getter
@Setter
public class DsMessageEntity {

    @Id
    private Long id;

    @Column(name = "recipient_ds_id")
    private String recipientId;

    @Column(name = "sender_ds_id")
    private String senderId;

    @Column(name = "sender_name")
    private String senderName;

    @Column(name = "sender_address")
    private String senderAddress;

    private String subject;

    private String message;

    @Column(name = "attachement_size")
    private int attachementSize;

    @Column(name = "delivery_time")
    private LocalDateTime deliveryTime;

    @Column(name = "acceptance_time")
    private LocalDateTime acceptanceTime;

    @Column(name = "created_in_app_time")
    private LocalDateTime createdInAppTime;

    @OneToMany(fetch = FetchType.EAGER)
    @JoinColumn(name = "message_id", updatable = false, insertable = false)
    private List<DsMessageAttachementEntity> dsMessageAttachementEntityList;

    private Boolean viewed;
    private Boolean deleted;

    @Enumerated(EnumType.STRING)
    @Column(name = "message_type")
    private DsMessageType type;

}
