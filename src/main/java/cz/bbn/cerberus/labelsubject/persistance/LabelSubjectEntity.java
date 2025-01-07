package cz.bbn.cerberus.labelsubject.persistance;


import cz.bbn.cerberus.label.persistance.LabelEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Entity
@Table(name = "label_subject", schema = "enums")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class LabelSubjectEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "subject_id")
    private String subjectId;

    @OneToOne
    @JoinColumn(name = "label_id", referencedColumnName = "id")
    private LabelEntity labelEntity;

    private String value;

}
