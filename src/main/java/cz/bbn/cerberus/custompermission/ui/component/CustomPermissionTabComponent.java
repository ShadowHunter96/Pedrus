package cz.bbn.cerberus.custompermission.ui.component;

import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.custompermission.dto.CustomUserPermissionDto;
import cz.bbn.cerberus.custompermission.ui.CustomPermissionViewListener;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public class CustomPermissionTabComponent extends TabSimpleComponent {

    private final CustomPermissionViewListener listener;
    private final FormLayout formLayout;

    private CustomPermissionDragAndDropComponent dragAndDropComponent;

    public CustomPermissionTabComponent(
            CustomPermissionViewListener listener,
            Set<String> permissionSet,
            List<String> objectList,
            List<UserDto> userList,
            String object) {
        this.listener = listener;
        setSizeFull();
        permissionSet.add(CustomPermissionService.ALL_PERMISSION);

        formLayout = new FormLayout();
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);


        objectList.add(CustomPermissionService.ALL_PERMISSION);
        ComboBox<UserDto> userComboBox = new ComboBox<>();
        userComboBox.setItemLabelGenerator(UserDto::getName);
        userComboBox.setItems(userList);
        userComboBox.setWidth(CssVariables.COMBOBOX_LARGE_WIDTH.getValue());
        userComboBox.setLabel(Transl.get("User"));

        ComboBox<String> objectComboBox = new ComboBox<>();
        objectComboBox.setItems(objectList);
        objectComboBox.setWidth(CssVariables.COMBOBOX_LARGE_WIDTH.getValue());
        objectComboBox.setLabel(Transl.get("Object"));

        if (!userList.isEmpty() && !objectList.isEmpty()) {
            userComboBox.setValue(userList.get(0));
            objectComboBox.setValue(objectList.get(0));
            loadGrid(permissionSet, userList.get(0), objectList.get(0), object);
        }

        userComboBox.addValueChangeListener(event ->
                loadGrid(permissionSet, event.getValue(), objectComboBox.getValue(), object));
        objectComboBox.addValueChangeListener(event ->
                loadGrid(permissionSet, userComboBox.getValue(), event.getValue(), object));

        formLayout.add(userComboBox, objectComboBox);
        formLayout.getElement().getStyle().set("align-self", "center");
        add(formLayout, dragAndDropComponent);
    }

    private void loadGrid(Set<String> permissionGridSet, UserDto user,
                          String objectId, String object) {
        removeAll();
        Set<CustomUserPermissionDto> rightGrid = listener.getUserPermissions(object, user, objectId);
        Set<CustomUserPermissionDto> leftGrid = new HashSet<>();

        for (String permissionId : permissionGridSet) {
            boolean contains = false;
            for (CustomUserPermissionDto ownedPermission : rightGrid) {
                if (ownedPermission.getPermissionId().equals(permissionId)
                        && ownedPermission.getObjectId().equals(objectId)) {
                    contains = true;
                    break;
                }
            }
            if (!contains) {
                leftGrid.add(new CustomUserPermissionDto(object, permissionId, user.getId(),
                        objectId, Permission.valueOfOrNotExists(permissionId).canView()));
            }
        }

        List<CustomUserPermissionDto> listRightGrid = new ArrayList<>(rightGrid);
        listRightGrid.sort(Comparator.comparing(CustomUserPermissionDto::getPermissionId));
        rightGrid = new LinkedHashSet<>(listRightGrid);

        List<CustomUserPermissionDto> listLeftGrid = new ArrayList<>(leftGrid);
        listLeftGrid.sort(Comparator.comparing(CustomUserPermissionDto::getPermissionId));
        leftGrid = new LinkedHashSet<>(listLeftGrid);

        dragAndDropComponent = new CustomPermissionDragAndDropComponent(
                listener, rightGrid, leftGrid, user, object, objectId);

        add(formLayout, dragAndDropComponent);
    }
}
