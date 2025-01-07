package cz.bbn.cerberus.role.persistance.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "permission_role", schema = "security")
@NoArgsConstructor
@Getter
@Setter
public class RoleHasPermissionEntity {

    @EmbeddedId
    private RoleHasPermissionId id;

    public RoleHasPermissionEntity(RoleHasPermissionId id) {
        this.id = id;
    }

}
