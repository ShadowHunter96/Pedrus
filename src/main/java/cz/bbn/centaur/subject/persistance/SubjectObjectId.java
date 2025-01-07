package cz.bbn.cerberus.subject.persistance;

import cz.bbn.cerberus.commons.enums.ObjectType;
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
public class SubjectObjectId implements Serializable {

    @Column(name = "subject_id")
    private String subjectId;

    @Column(name = "object_id")
    private String objectId;

    @Column(name = "object_type")
    @Enumerated(EnumType.STRING)
    private ObjectType objectType;
}
