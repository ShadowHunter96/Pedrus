package cz.bbn.cerberus.contactperson.persistance.entity;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Getter
@Setter
@Entity
@Table(name = "contact_person_by_object", schema = "sales")
public class ContactPersonByObjectEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @OneToOne
    @JoinColumn(name = "contact_person_id", referencedColumnName = "id")
    private ContactPersonSimpleEntity contactPerson;

    @Column(name = "object_type")
    private String objectType;

    @Column(name = "added_to_object")
    private String addedToObjectId;
}
