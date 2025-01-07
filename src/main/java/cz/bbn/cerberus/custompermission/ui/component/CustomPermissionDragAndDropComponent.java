package cz.bbn.cerberus.custompermission.ui.component;

import cz.bbn.cerberus.commons.component.ui.AppDragAndDrop;
import cz.bbn.cerberus.custompermission.dto.CustomUserPermissionDto;
import cz.bbn.cerberus.custompermission.ui.CustomPermissionViewListener;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;

import java.util.HashSet;
import java.util.Set;

public class CustomPermissionDragAndDropComponent extends AppDragAndDrop<CustomUserPermissionDto> {

    private static final String PERMISSION_ID = "permissionId";

    private final CustomPermissionViewListener listener;

    private final String object;
    private final UserDto user;
    private final String objectId;

    public CustomPermissionDragAndDropComponent(
            CustomPermissionViewListener listener,
            Set<CustomUserPermissionDto> permissionRightSet,
            Set<CustomUserPermissionDto> permissionLeftSet,
            UserDto user, String object, String objectId) {
        super(permissionRightSet, permissionLeftSet,
                PERMISSION_ID, Transl.get("Permission code"),
                Transl.get("List of unassigned custom permissions"),
                Transl.get("List of assigned custom permissions"),
                CustomUserPermissionDto.class, true);
        this.listener = listener;
        this.user = user;
        this.object = object;
        this.objectId = objectId;
        setHeightFull();
    }


    @Override
    public String getSearchCompareVariable(CustomUserPermissionDto dto) {
        return dto.getPermissionId();
    }

    @Override
    public void onChangeAction(Set<CustomUserPermissionDto> rightSet) {
        listener.setCustomPermissionDtoList(new HashSet<>(rightSet.stream().toList()), object, user, objectId);
    }

}
