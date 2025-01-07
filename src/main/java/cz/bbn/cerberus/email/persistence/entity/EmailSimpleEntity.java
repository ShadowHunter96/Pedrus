package cz.bbn.cerberus.email.persistence.entity;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import java.time.LocalDateTime;

@Entity
@Table(name = "email_msg", schema = "other")
@Getter
@Setter
public class EmailSimpleEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String subject;
    private String body;
    private String sender;

    @Column(name = "receive_time")
    private LocalDateTime dateAndTime;

    @Column(name = "no_of_attachments")
    private int noOfAttachments;

    @Column(name = "customer_id")
    private String customer;

    @Column(name = "entity_type")
    private String entityType;

    @Column(name = "entity_id")
    private String entityId;
}
