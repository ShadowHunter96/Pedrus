package cz.bbn.cerberus.document.persistance.entity;


import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "document_by_object", schema = "other")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class DocumentByObjectEntity {

    @EmbeddedId
    private DocumentByObjectId id;
}
