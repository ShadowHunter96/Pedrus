package cz.bbn.cerberus.dsmessage.persistance.entity;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Basic;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "ds_attachement", schema = "backoffice")
@Getter
@Setter
public class DsMessageAttachementEntity {

    @Id
    private Long id;

    @Column(name = "message_id")
    private Long messageId;

    private String name;

    @Basic(fetch = FetchType.LAZY)
    private byte[] file;
}
