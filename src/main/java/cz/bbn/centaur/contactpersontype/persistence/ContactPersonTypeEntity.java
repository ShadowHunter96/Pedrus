package cz.bbn.cerberus.contactpersontype.persistence;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "contact_person_type", schema = "enums")
@Getter
@Setter
@NoArgsConstructor
public class ContactPersonTypeEntity {

    @Id
    private String id;

    private String name;
    private Boolean allowed;

    public ContactPersonTypeEntity(String id) {
        this.id = id;
    }
}
