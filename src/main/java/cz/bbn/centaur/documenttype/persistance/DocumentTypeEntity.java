package cz.bbn.cerberus.documenttype.persistance;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "document_type", schema = "enums")
@Getter
@Setter
@NoArgsConstructor
public class DocumentTypeEntity {

    @Id
    private String id;

    private String name;
    private String description;
    private Boolean allowed;

    @Column(name = "allowed_formats")
    private String allowedFormats;

    public DocumentTypeEntity(String id) {
        this.id = id;
    }
}
