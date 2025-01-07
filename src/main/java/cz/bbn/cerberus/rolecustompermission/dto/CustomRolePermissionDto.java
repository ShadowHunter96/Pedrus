package cz.bbn.cerberus.rolecustompermission.dto;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@ToString
@EqualsAndHashCode
public class CustomRolePermissionDto {

    private String objectName;

    private String permissionId;

    private String roleId;

    private String objectId;

    private boolean canView;
}
