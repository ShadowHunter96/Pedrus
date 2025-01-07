package cz.bbn.cerberus.permissionmanagement.ui.component.tab;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.permissionmanagement.dto.PermissionExistsDto;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.role.dto.RoleHasPermissionDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class PermissionTab extends TabSimpleComponent {

    private final Map<DomainEnum, List<Permission>> permissionMap;
    private final List<RoleDto> roleList;
    private final Map<String, VerticalLayout> roleColumnMap = new HashMap<>();
    private final Map<String, VerticalLayout> roleNameDivMap = new HashMap<>();
    private final Map<String, Map<String, Map<PermissionExistsDto, Checkbox>>> roleDomainPermissionValueMap =
            new HashMap<>();

    public PermissionTab(Map<DomainEnum, List<Permission>> permissionMap, List<RoleDto> roleList) {
        this.permissionMap = permissionMap;
        this.roleList = roleList;
        initTab();
    }

    private void initTab() {
        MultiSelectComboBox<RoleDto> roleSelect = new MultiSelectComboBox<>(Transl.get("Roles"));
        roleList.sort(Comparator.comparing(RoleDto::getId));
        roleSelect.setItems(roleList);
        roleSelect.setItemLabelGenerator(RoleDto::getId);
        roleSelect.setWidthFull();
        add(roleSelect);
        roleSelect.addValueChangeListener(e -> {
            for (VerticalLayout column : roleColumnMap.values()) {
                column.setVisible(false);
            }
            for (VerticalLayout line : roleNameDivMap.values()) {
                line.setVisible(false);
            }
            for (RoleDto roleDto : roleSelect.getSelectedItems()) {
                VerticalLayout column = roleColumnMap.get(roleDto.getId());
                column.setVisible(true);
                VerticalLayout line = roleNameDivMap.get(roleDto.getId());
                line.setVisible(true);
            }
        });

        generateFirstLine();

        HorizontalLayout gridLayout = new HorizontalLayout();
        gridLayout.setMargin(false);
        gridLayout.setPadding(false);
        gridLayout.setSpacing(false);
        gridLayout.setHeightFull();
        gridLayout.setAlignItems(Alignment.START);
        gridLayout.setDefaultVerticalComponentAlignment(Alignment.START);

        generateFirstColumn(gridLayout);

        for (RoleDto role : roleList) {
            generateRoleColumn(role, gridLayout);
        }

        VerticalLayout div = new VerticalLayout();
        div.setSizeFull();
        div.setMargin(false);
        div.setPadding(false);
        div.setSpacing(false);
        div.getElement().getStyle().set("overflow", "auto");
        div.add(generateFirstLine(), gridLayout);
        add(div);
    }

    private HorizontalLayout generateFirstLine() {
        HorizontalLayout line = new HorizontalLayout();
        line.setMargin(false);
        line.setPadding(false);
        line.setSpacing(false);
        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.setMargin(false);
        verticalLayout.setPadding(false);
        verticalLayout.setSpacing(false);
        verticalLayout.setWidth("21em");
        verticalLayout.setMinWidth("21em");
        verticalLayout.setHeight("2em");
        Div emptyDiv = new Div();
        emptyDiv.addClassName("grid-block");
        verticalLayout.add(emptyDiv);
        verticalLayout.getElement().getStyle().set("position", "sticky").set("top", "0").set("left", "0")
                .set("background-color", "white").set("z-index", "1");
        line.add(verticalLayout);
        line.getStyle().set("position", "sticky").set("top", "0")
                .set("background-color", "white").set("z-index", "1");

        for (RoleDto role : roleList) {
            VerticalLayout outerLayout = new VerticalLayout();
            outerLayout.setAlignItems(Alignment.CENTER);
            outerLayout.setHeight("2em");
            outerLayout.setMargin(false);
            outerLayout.setPadding(false);
            outerLayout.setSpacing(false);
            outerLayout.setWidth("10em");
            outerLayout.setMinWidth("10em");
            outerLayout.setVisible(false);
            Div roleName = new Div();
            roleName.add(new Label(role.getId()));
            roleName.addClassNames("grid-block", "align-center-simple");
            outerLayout.add(roleName);
            line.add(outerLayout);
            roleNameDivMap.put(role.getId(), outerLayout);
        }
        return line;
    }

    private void generateFirstColumn(HorizontalLayout gridLayout) {

        VerticalLayout firstColumn = new VerticalLayout();
        firstColumn.setMargin(false);
        firstColumn.setPadding(false);
        firstColumn.setSpacing(false);
        firstColumn.setWidth("21em");
        firstColumn.setMinWidth("21em");
        firstColumn.getElement().getStyle().set("position", "sticky").set("left", "0")
                .set("background-color", "white").set("z-index", "1").set("margin-top", "calc(-1.5em - 10px)");

        Div emptyDiv = new Div();
        emptyDiv.setMinWidth("calc(21em + 10px)");
        emptyDiv.setMinHeight("calc(1.5em + 13px)");
        emptyDiv.getElement().getStyle().set("position", "sticky").set("top", "0").set("left", "0")
                .set("background-color", "white").set("margin-top", "-3px");
        firstColumn.add(emptyDiv);

        for (DomainEnum domainEnum : DomainEnum.getPermissionDomainSet()) {
            Div domainName = new Div();
            domainName.addClassName("grid-block");
            domainName.add(new Label(Transl.get(domainEnum.getNameForTransl())));
            Label all = new Label(Transl.get("All"));
            Label readOnly = new Label(Transl.get("Read only"));
            Label permissions = new Label(Transl.get("Permissions"));
            Div allDiv = new Div();
            Div readOnlyDiv = new Div();
            Div permissionsDiv = new Div();
            allDiv.addClassName("grid-block");
            readOnlyDiv.addClassName("grid-block");
            permissionsDiv.addClassName("grid-block");
            allDiv.add(all);
            readOnlyDiv.add(readOnly);
            permissionsDiv.add(permissions);
            all.getElement().getStyle().set("padding-left", "2em");
            readOnly.getElement().getStyle().set("padding-left", "2em");
            permissions.getElement().getStyle().set("padding-left", "2em");
            firstColumn.add(domainName);
            firstColumn.add(allDiv);
            firstColumn.add(readOnlyDiv);
            firstColumn.add(permissionsDiv);
            for (Permission permission : permissionMap.get(domainEnum)) {
                Div itemDiv = new Div();
                itemDiv.addClassName("grid-block");
                Label item = new Label(Transl.get(permission.getTitle()));
                item.getElement().setProperty(TextValues.TITLE, Transl.get(permission.getDescription())
                        .concat("\n").concat(permission.name()));
                item.getElement().getStyle().set("padding-left", "4em");
                itemDiv.add(item);
                firstColumn.add(itemDiv);
            }
        }
        gridLayout.add(firstColumn);
    }

    private void generateRoleColumn(RoleDto role, HorizontalLayout gridLayout) {
        Map<String, Map<PermissionExistsDto, Checkbox>> domainMap = new HashMap<>();
        VerticalLayout column = new VerticalLayout();
        column.setAlignItems(Alignment.CENTER);
        column.setHeightFull();
        column.setMargin(false);
        column.setPadding(false);
        column.setSpacing(false);
        column.setWidth("10em");
        column.setMinWidth("10em");

        Set<String> permissionSet = new HashSet<>();
        if (role.getRoleHasPermissionSet() != null) {
            for (RoleHasPermissionDto roleHasPermissionDto : role.getRoleHasPermissionSet()) {
                permissionSet.add(roleHasPermissionDto.getPermissionId());
            }
        }

        for (DomainEnum domainEnum : DomainEnum.getPermissionDomainSet()) {
            Map<PermissionExistsDto, Checkbox> permissionStringMap = new HashMap<>();
            Div domainDiv = new Div();
            domainDiv.addClassNames("grid-block", "align-center-simple");
            column.add(domainDiv);
            Checkbox all = new Checkbox();
            Checkbox readOnly = new Checkbox();
            all.addValueChangeListener(e -> {
                setAll(e.getValue(), permissionStringMap, e.isFromClient());
                checkReadOnly(readOnly, permissionStringMap, e.isFromClient());
            });
            readOnly.addValueChangeListener(e -> {
                setReadOnly(e.getValue(), permissionStringMap, e.isFromClient());
                checkAll(all, permissionStringMap, e.isFromClient());
                checkReadOnly(readOnly, permissionStringMap, e.isFromClient());
            });
            Div allDiv = new Div();
            Div readOnlyDiv = new Div();
            Div permissionsDiv = new Div();
            allDiv.addClassNames("grid-block", "align-center-simple");
            readOnlyDiv.addClassNames("grid-block", "align-center-simple");
            permissionsDiv.addClassNames("grid-block", "align-center-simple");
            allDiv.add(all);
            readOnlyDiv.add(readOnly);
            column.add(domainDiv);
            column.add(allDiv);
            column.add(readOnlyDiv);
            column.add(permissionsDiv);
            permissionStringMap.put(new PermissionExistsDto(false, "ALL"), all);
            permissionStringMap.put(new PermissionExistsDto(false, "READ_ONLY"), readOnly);
            for (Permission permission : permissionMap.get(domainEnum)) {
                Div itemDiv = new Div();
                itemDiv.addClassNames("grid-block", "align-center-simple");
                Checkbox item = new Checkbox();
                item.setValue(permissionSet.contains(permission.name()));
                item.addValueChangeListener(e -> {
                    checkAll(all, permissionStringMap, e.isFromClient());
                    checkReadOnly(readOnly, permissionStringMap, e.isFromClient());
                });
                itemDiv.add(item);
                column.add(itemDiv);
                permissionStringMap.put(new PermissionExistsDto(true, permission.name()), item);
            }
            domainMap.put(domainEnum.getValue(), permissionStringMap);
            checkAll(all, permissionStringMap, true);
            checkReadOnly(readOnly, permissionStringMap, true);
        }
        column.setVisible(false);
        roleColumnMap.put(role.getId(), column);
        gridLayout.add(column);
        roleDomainPermissionValueMap.put(role.getId(), domainMap);
    }

    public Map<String, Set<RoleHasPermissionDto>> getRoleValues() {
        Map<String, Set<RoleHasPermissionDto>> rolePermissionMap = new HashMap<>();
        for (RoleDto roleDto : roleList) {
            addToRolePermissionMap(roleDto, rolePermissionMap);
        }
        return rolePermissionMap;
    }

    private void addToRolePermissionMap(RoleDto roleDto, Map<String, Set<RoleHasPermissionDto>> rolePermissionMap) {
        Set<RoleHasPermissionDto> roleHasPermissionDtoSet = new HashSet<>();
        Map<String, Map<PermissionExistsDto, Checkbox>> roleMap = roleDomainPermissionValueMap.get(roleDto.getId());
        for (Map<PermissionExistsDto, Checkbox> domainMap : roleMap.values()) {
            for (Map.Entry<PermissionExistsDto, Checkbox> permissionExistsEntry : domainMap.entrySet()) {
                if (permissionExistsEntry.getKey().isPermission()
                        && Boolean.TRUE.equals(permissionExistsEntry.getValue().getValue())) {
                    RoleHasPermissionDto roleHasPermissionDto = new RoleHasPermissionDto();
                    roleHasPermissionDto.setPermissionId(permissionExistsEntry.getKey().getPermissionName());
                    roleHasPermissionDtoSet.add(roleHasPermissionDto);
                }
            }
        }
        rolePermissionMap.put(roleDto.getId(), roleHasPermissionDtoSet);
    }

    private void checkAll(Checkbox allCheckbox, Map<PermissionExistsDto, Checkbox> permMap, boolean perform) {
        if (perform) {
            for (Map.Entry<PermissionExistsDto, Checkbox> permissionExistsEntry : permMap.entrySet()) {
                if (permissionExistsEntry.getKey().isPermission()
                        && !Boolean.TRUE.equals(permissionExistsEntry.getValue().getValue())) {
                    allCheckbox.setValue(false);
                    return;
                }
            }
            allCheckbox.setValue(true);
        }
    }

    private void checkReadOnly(Checkbox readOnly, Map<PermissionExistsDto, Checkbox> permMap, boolean perform) {
        if (perform) {
            for (Map.Entry<PermissionExistsDto, Checkbox> permissionExistsEntry : permMap.entrySet()) {
                if (permissionExistsEntry.getKey().isPermission()) {
                    Permission permission = Permission.valueOf(permissionExistsEntry.getKey().getPermissionName());
                    boolean value = permissionExistsEntry.getValue().getValue();
                    if ((permission.canView() && !value) || (!permission.canView() && value)) {
                        readOnly.setValue(false);
                        return;
                    }
                }
            }
            readOnly.setValue(true);
        }
    }

    private void setAll(boolean value, Map<PermissionExistsDto, Checkbox> permMap, boolean perform) {
        if (perform) {
            for (Map.Entry<PermissionExistsDto, Checkbox> permissionExistsEntry : permMap.entrySet()) {
                if (permissionExistsEntry.getKey().isPermission()) {
                    permissionExistsEntry.getValue().setValue(value);
                }
            }
        }
    }

    private void setReadOnly(boolean value, Map<PermissionExistsDto, Checkbox> permMap, boolean perform) {
        if (value && perform) {
            for (Map.Entry<PermissionExistsDto, Checkbox> permissionExistsEntry : permMap.entrySet()) {
                if (permissionExistsEntry.getKey().isPermission()) {
                    Permission permission = Permission.valueOf(permissionExistsEntry.getKey().getPermissionName());
                    permissionExistsEntry.getValue().setValue(permission.canView());
                }
            }
        }
    }

}
