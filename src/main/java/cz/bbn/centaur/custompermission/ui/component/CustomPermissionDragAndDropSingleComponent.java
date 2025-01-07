package cz.bbn.cerberus.custompermission.ui.component;

import cz.bbn.cerberus.commons.component.ui.AppDragAndDrop;
import cz.bbn.cerberus.custompermission.dto.PermUserDto;
import cz.bbn.cerberus.custompermission.ui.ChangeAffectedUsersAction;
import cz.bbn.cerberus.translation.Transl;

import java.util.Set;

public class CustomPermissionDragAndDropSingleComponent extends AppDragAndDrop<PermUserDto> {

    private static final String PERMISSION_ID = "name";

    private final ChangeAffectedUsersAction listener;

    public CustomPermissionDragAndDropSingleComponent(
            ChangeAffectedUsersAction listener,
            Set<PermUserDto> permissionRightSet,
            Set<PermUserDto> permissionLeftSet) {
        super(permissionRightSet, permissionLeftSet,
                PERMISSION_ID, Transl.get("name"),
                Transl.get("List of unassigned users"), Transl.get("Users with permission"),
                PermUserDto.class, true);
        this.listener = listener;
        setHeightFull();
        setMargin(false);
        setPadding(false);
    }


    @Override
    public String getSearchCompareVariable(PermUserDto userDto) {
        return String.valueOf(userDto.getName());
    }

    @Override
    public void onChangeAction(Set<PermUserDto> rightSet) {
        listener.changeAffectedUserSet(rightSet);
    }
}
