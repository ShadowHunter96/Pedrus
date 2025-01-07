package cz.bbn.cerberus.subject.persistance;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.io.Serializable;

@Entity
@Table(name = "subject_by_object", schema = "sales")
@Getter
@Setter
@ToString
@NoArgsConstructor
public class SubjectByObjectEntity implements Serializable {

    @EmbeddedId
    private SubjectObjectId id;

    @OneToOne
    @JoinColumn(name = "subject_id", referencedColumnName = "id", insertable = false, updatable = false)
    private SubjectEntity subjectEntity;

    public SubjectByObjectEntity(SubjectObjectId id) {
        this.id = id;
    }
}
