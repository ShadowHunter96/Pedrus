package cz.bbn.cerberus.document.persistance.entity;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.mongodb.core.mapping.Document;

import javax.persistence.Id;

@Document("document_file")
@Getter
@Setter
public class DocumentFileMongoEntity {

    @Id
    private String id;

    private String name;

    private byte[] file;
}
