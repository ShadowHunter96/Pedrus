package cz.bbn.cerberus.custompermission.ui.component;

import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Span;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.custompermission.dto.CustomPermType;
import cz.bbn.cerberus.custompermission.dto.PermUserDto;
import cz.bbn.cerberus.custompermission.ui.ChangeAffectedUsersAction;
import cz.bbn.cerberus.user.dto.UserDto;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class CustomPermissionSinglePermissionTabComponent extends TabSimpleComponent {

    private final ChangeAffectedUsersAction listener;

    private final Div gridLayout;

    public CustomPermissionSinglePermissionTabComponent(
            ChangeAffectedUsersAction listener,
            String permissionName,
            String objectName,
            List<UserDto> userList,
            String objectId, String title) {
        this.listener = listener;
        this.gridLayout = new Div();
        gridLayout.setSizeFull();
        setHeightFull();

        add(new Span(title), gridLayout);
        loadGrid(userList, objectName, objectId, permissionName);
    }

    private void loadGrid(List<UserDto> userList, String objectName, String objectId, String permissionId) {
        gridLayout.removeAll();
        Set<PermUserDto> rightGrid = new HashSet<>();
        Set<PermUserDto> leftGrid = new HashSet<>();

        Map<CustomPermType, Set<PermUserDto>> userMap = SecurityUtils.getCustomUserPermMap(
                objectName, objectId, permissionId, userList);

        for (Map.Entry<CustomPermType, Set<PermUserDto>> entry : userMap.entrySet()) {
            if (entry.getKey() == CustomPermType.PERMANENT_PERM || entry.getKey() == CustomPermType.CHANGEABLE_PERM) {
                rightGrid.addAll(entry.getValue());
            } else {
                leftGrid.addAll(entry.getValue());
            }
        }

        List<PermUserDto> listRightGrid = new ArrayList<>(rightGrid);
        listRightGrid.sort(Comparator.comparing(PermUserDto::getName));
        rightGrid = new LinkedHashSet<>(listRightGrid);

        List<PermUserDto> listLeftGrid = new ArrayList<>(leftGrid);
        listLeftGrid.sort(Comparator.comparing(PermUserDto::getName));
        leftGrid = new LinkedHashSet<>(listLeftGrid);

        gridLayout.add(new CustomPermissionDragAndDropSingleComponent(listener, rightGrid, leftGrid));
    }
}
