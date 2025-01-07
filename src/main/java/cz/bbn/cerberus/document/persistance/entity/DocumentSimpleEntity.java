package cz.bbn.cerberus.document.persistance.entity;


import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import java.util.List;

@Entity
@Table(name = "document", schema = "other")
@Getter
@Setter
public class DocumentSimpleEntity {

    @Id
    private String name;

    @Column(name = "file_type")
    private String fileType;

    @Column(name = "document_type")
    private String documentType;

    @OneToMany(fetch = FetchType.LAZY)
    @JoinColumn(name = "document_name", updatable = false, insertable = false)
    private List<DocumentByObjectEntity> documentByObjectEntity;

    private Long size;

    private Boolean deleted;
}
