package cz.bbn.cerberus.document.persistance.entity;

import cz.bbn.cerberus.document.DocumentObjectEnum;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import java.io.Serializable;

@Embeddable
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class DocumentByObjectId implements Serializable {

    @Column(name = "document_name")
    private String documentName;

    @Column(name = "object_id")
    private String objectId;

    @Column(name = "object_type")
    @Enumerated(EnumType.STRING)
    private DocumentObjectEnum objectType;

}
