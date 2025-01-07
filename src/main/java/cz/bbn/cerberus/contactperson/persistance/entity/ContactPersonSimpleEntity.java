package cz.bbn.cerberus.contactperson.persistance.entity;

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

@Getter
@Setter
@Entity
@Table(name = "contact_person", schema = "sales")
public class ContactPersonSimpleEntity {

    @Id
    private String id;

    @Column(name = "first_name")
    private String firstName;

    @Column(name = "last_name")
    private String lastName;

    @OneToOne(fetch = FetchType.EAGER)
    @NotFound(action = NotFoundAction.IGNORE)
    @JoinColumn(name = "phone_prefix_id")
    private PhonePrefixEntity phonePrefixEntity;

    private String email;
    private String phone;
    private Boolean deleted;

}
