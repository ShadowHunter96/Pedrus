package cz.bbn.cerberus.custompermission.dto;

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
public class CustomUserPermissionDto {

    private String objectName;

    private String permissionId;

    private Long userId;

    private String objectId;

    private boolean canView;
}
