package cz.bbn.cerberus.role.ui.component;

import cz.bbn.cerberus.commons.component.ui.AppDragAndDrop;
import cz.bbn.cerberus.role.dto.RoleHasPermissionDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.Set;

public class RoleDragAndDropComponent extends AppDragAndDrop<RoleHasPermissionDto> {

    private static final String PERMISSION_ID = "permissionId";

    public RoleDragAndDropComponent(Set<RoleHasPermissionDto> rightSet, Set<RoleHasPermissionDto> leftSet) {
        super(rightSet, leftSet,
                PERMISSION_ID, Transl.get("Role code"),
                Transl.get("List of unassigned permissions"), Transl.get("Owned permissions"),
                RoleHasPermissionDto.class, true);
    }

    @Override
    public String getSearchCompareVariable(RoleHasPermissionDto dto) {
        return dto.getPermissionId();
    }
}
