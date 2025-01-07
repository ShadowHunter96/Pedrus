package cz.bbn.cerberus.contactperson.persistance.entity;

import cz.bbn.cerberus.contactpersontype.persistence.ContactPersonTypeEntity;
import cz.bbn.cerberus.phoneprefix.persistance.PhonePrefixEntity;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.NotFound;
import org.hibernate.annotations.NotFoundAction;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Entity
@Table(name = "contact_person", schema = "sales")
@Getter
@Setter
public class ContactPersonEntity {

    @Id
    private String id;

    @Column(name = "first_name")
    private String firstName;

    @Column(name = "last_name")
    private String lastName;

    private String description;
    private String email;

    @OneToOne(fetch = FetchType.EAGER)
    @NotFound(action = NotFoundAction.IGNORE)
    @JoinColumn(name = "phone_prefix_id")
    private PhonePrefixEntity phonePrefixEntity;

    private String phone;

    @OneToOne(fetch = FetchType.EAGER)
    @NotFound(action = NotFoundAction.IGNORE)
    @JoinColumn(name = "phone_prefix_id_2")
    private PhonePrefixEntity phonePrefix2Entity;

    @Column(name = "phone_2")
    private String phone2;

    @Column(name = "other_contacts")
    private String otherContacts;

    @OneToOne(fetch = FetchType.EAGER)
    @NotFound(action = NotFoundAction.IGNORE)
    @JoinColumn(name = "contact_person_position_type")
    private ContactPersonTypeEntity contactPersonTypeEntity;

    @Column(name = "contact_person_position_text")
    private String contactPersonPosition;

    private Boolean deleted;
}
