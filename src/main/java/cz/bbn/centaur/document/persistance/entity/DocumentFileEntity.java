package cz.bbn.cerberus.document.persistance.entity;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "document_file", schema = "other")
@Getter
@Setter
public class DocumentFileEntity {

    @Id
    private String name;

    private byte[] file;
}
