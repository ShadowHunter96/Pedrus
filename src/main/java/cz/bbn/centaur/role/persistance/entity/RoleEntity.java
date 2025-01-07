package cz.bbn.cerberus.role.persistance.entity;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "user_role", schema = "security")
@Getter
@Setter
public class RoleEntity {

    @Id
    private String id;

    private String name;

    @Column(name = "back_office")
    private Boolean backOffice;

    private Boolean infrastructure;

}
