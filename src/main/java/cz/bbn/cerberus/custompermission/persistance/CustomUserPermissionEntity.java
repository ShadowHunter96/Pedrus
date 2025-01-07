package cz.bbn.cerberus.custompermission.persistance;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "custom_permission", schema = "security")
@Getter
@Setter
@ToString
public class CustomUserPermissionEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private long id;

    @Column(name = "object_name")
    private String objectName;

    @Column(name = "permission_id")
    private String permissionId;

    @Column(name = "user_id")
    private Long userId;

    @Column(name = "object_id")
    private String objectId;

    @Column(name = "can_view")
    private Boolean canView;
}
