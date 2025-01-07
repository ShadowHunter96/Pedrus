package cz.bbn.cerberus.document.persistance.entity;

import cz.bbn.cerberus.documenttype.persistance.DocumentTypeEntity;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.NotFound;
import org.hibernate.annotations.NotFoundAction;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "document", schema = "other")
@Getter
@Setter
public class DocumentEntity {

    @Id
    private String name;

    @Column(name = "file_type")
    private String fileType;

    @OneToOne(fetch = FetchType.EAGER)
    @NotFound(action = NotFoundAction.IGNORE)
    @JoinColumn(name = "document_type")
    private DocumentTypeEntity documentTypeEntity;

    @OneToMany(fetch = FetchType.EAGER)
    @JoinColumn(name = "document_name", updatable = false, insertable = false)
    private List<DocumentByObjectEntity> documentByObjectEntity;

    private Long size;

    private Boolean deleted;

    @Column(name = "deleted_date")
    private LocalDateTime deletedDate;

}
