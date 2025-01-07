package cz.bbn.cerberus.role.persistance.entity;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import java.io.Serializable;

@Embeddable
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class RoleHasPermissionId implements Serializable {

    @Column(name = "permission_id")
    private String permissionId;

    @Column(name = "role_id")
    private String roleId;
}
