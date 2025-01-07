package cz.bbn.cerberus.email.dto;

import lombok.Getter;
import lombok.Setter;

import java.io.InputStream;
import java.io.Serializable;
import java.time.LocalDateTime;

@Getter
@Setter
public class EmailDto implements Serializable {

    private Long id;

    private String subject;
    private String body;
    private String sender;

    private LocalDateTime dateAndTime;
    private int noOfAttachments;

    private InputStream file;

    private String customer;
    private String entityType;
    private String entityId;
}
