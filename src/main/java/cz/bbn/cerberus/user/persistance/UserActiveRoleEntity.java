package cz.bbn.cerberus.user.persistance;

import cz.bbn.cerberus.role.persistance.entity.RoleEntity;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Entity
@Table(name = "user_active_role", schema = "security")
@Getter
@Setter
@NoArgsConstructor
public class UserActiveRoleEntity {

    @EmbeddedId
    private UserActiveRoleId id;

    @OneToOne
    @JoinColumn(name = "role_id", referencedColumnName = "id", insertable = false, updatable = false)
    private RoleEntity roleEntity;
}
