package cz.bbn.cerberus.permissionmanagement.ui.component.tab;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.interfaces.ChangePageAction;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.permissionmanagement.ui.component.CustomPermissionGridComponent;
import cz.bbn.cerberus.permissionmanagement.ui.component.PageChangeComponent;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static cz.bbn.cerberus.permissionmanagement.ui.component.CustomPermissionGridComponent.PAGE_SIZE;

public class CustomRolePermissionTab extends TabSimpleComponent {

    private final Map<DomainEnum, List<Permission>> customPermisionMap;
    private final Map<String, List<String>> objectMap;
    private final Set<DomainEnum> domainSet;
    private final List<RoleDto> roleList;

    private final ComboBox<DomainEnum> domainComboBox = new ComboBox<>(Transl.get("Domain"));
    private final ComboBox<RoleDto> roleComboBox = new ComboBox<>(Transl.get("Role"));
    private final Checkbox ownedPermissions = new Checkbox(Transl.get("Owned permissions"));
    private final TextField search = new TextField(Transl.get("Search"));
    private final Label pageSizeLabel = new Label(Transl.get("Page size" + ": " + PAGE_SIZE));

    private Map<String, Map<String, Map<String, Set<String>>>> roleDomainEntityPermissionMap;

    private CustomPermissionGridComponent customPermissionGridComponent;
    private PageChangeComponent pageChangeComponent;

    public CustomRolePermissionTab(Map<DomainEnum, List<Permission>> customPermissionMap,
                                   Set<DomainEnum> domainSet, List<RoleDto> roleList,
                                   Map<String, List<String>> objectMap,
                                   Map<String, Map<String, Map<String, Set<String>>>> roleDomainEntityPermissionMap) {
        this.customPermisionMap = customPermissionMap;
        this.domainSet = domainSet;
        this.roleList = roleList;
        this.objectMap = objectMap;
        this.roleDomainEntityPermissionMap = roleDomainEntityPermissionMap;
        initTab();
    }

    private void initTab() {
        setSizeFull();
        getDomainsForCB();

        FormLayout formLayout = new FormLayout();
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);

        domainComboBox.setItems(getDomainsForCB());
        domainComboBox.setItemLabelGenerator(DomainEnum::getTranslatedName);
        roleComboBox.setItems(roleList);
        roleComboBox.setItemLabelGenerator(RoleDto::getId);

        ownedPermissions.setValue(true);

        formLayout.add(domainComboBox, roleComboBox, ownedPermissions, search, pageSizeLabel);
        add(formLayout);

        customPermissionGridComponent = new CustomPermissionGridComponent(
                customPermisionMap, objectMap, domainComboBox,
                roleDomainEntityPermissionMap, ownedPermissions);

        pageChangeComponent = new PageChangeComponent(getChangePageAction());

        domainComboBox.addValueChangeListener(e -> {
            if (e.getValue() != null && roleComboBox.getValue() != null) {
                generateGrid(1);
            }
        });

        roleComboBox.addValueChangeListener(e -> {
            if (domainComboBox.getValue() != null && e.getValue() != null) {
                generateGrid(1);
            }
        });
        ownedPermissions.addValueChangeListener(e -> {
            if (domainComboBox.getValue() != null && roleComboBox.getValue() != null) {
                generateGrid(1);
            }
        });

        search.addValueChangeListener(e -> {
            if (domainComboBox.getValue() != null && roleComboBox.getValue() != null) {
                generateGrid(1);
            }
        });

        add(customPermissionGridComponent);
        add(pageChangeComponent);
    }

    private void generateGrid(int page) {
        List<String> items = objectMap.get(domainComboBox.getValue().getValue());
        if (Boolean.TRUE.equals(ownedPermissions.getValue())) {
            items = getEntitiesWithPermission(
                    domainComboBox.getValue().getValue(), roleComboBox.getValue().getId());
        }
        if (search.getValue() != null && !search.getValue().trim().isEmpty()) {
            List<String> newItemList = new ArrayList<>();
            for (String item : items) {
                if (item.toLowerCase().contains(search.getValue().toLowerCase())) {
                    newItemList.add(item);
                }
            }
            items = newItemList;
        }
        int numberOfItems = items.size();
        customPermissionGridComponent.generateGrid(domainComboBox.getValue(),
                String.valueOf(roleComboBox.getValue().getId()),
                roleDomainEntityPermissionMap, page, numberOfItems, items);
        pageChangeComponent.generateComponent(page, getNumberOfPages(numberOfItems));
    }

    private List<String> getEntitiesWithPermission(String domain, String role) {
        if (roleDomainEntityPermissionMap.containsKey(role)
                && roleDomainEntityPermissionMap.get(role).containsKey(domain)) {
            if (roleDomainEntityPermissionMap.get(role).get(domain).containsKey(CustomPermissionService.ALL_PERMISSION)
                    && !roleDomainEntityPermissionMap.get(role).get(domain).get(
                    CustomPermissionService.ALL_PERMISSION).isEmpty()) {
                List<String> toReturnList = objectMap.get(domain);
                Collections.sort(toReturnList);
                return toReturnList;
            }
            if (!roleDomainEntityPermissionMap.get(role).get(domain).isEmpty()) {
                List<String> toReturnList = new ArrayList<>();
                Map<String, Set<String>> entityMap = roleDomainEntityPermissionMap.get(role).get(domain);
                for (Map.Entry<String, Set<String>> map : entityMap.entrySet()) {
                    if (map.getValue() != null && !map.getValue().isEmpty()) {
                        toReturnList.add(map.getKey());
                    }
                }
                Collections.sort(toReturnList);
                return toReturnList;
            }
        }
        return new ArrayList<>();
    }

    private Set<DomainEnum> getDomainsForCB() {
        Set<DomainEnum> toReturnDomainSet = new LinkedHashSet<>();
        for (DomainEnum domain : this.domainSet) {
            if (objectMap.containsKey(domain.getValue())) {
                toReturnDomainSet.add(domain);
            }
        }
        return toReturnDomainSet;
    }

    private int getNumberOfPages(int numberOfItems) {
        int remainder = numberOfItems % PAGE_SIZE;
        int noOfPages = numberOfItems / PAGE_SIZE;
        if (remainder > 0) {
            noOfPages++;
        }
        return noOfPages;
    }

    public Map<String, Map<String, Map<String, Set<String>>>> getRolePermMap() {
        return roleDomainEntityPermissionMap;
    }

    public Map<String, Set<String>> getChangedMap() {
        return customPermissionGridComponent.getItemDomainChanged();
    }

    public void resetChangedMap() {
        this.roleDomainEntityPermissionMap = new HashMap<>(SecurityUtils.getRoleDomainEntityPermissionMap());
        this.customPermissionGridComponent.resetChangedMap();
        if (roleComboBox.getValue() != null && domainComboBox.getValue() != null) {
            generateGrid(1);
        }
    }

    public ChangePageAction getChangePageAction() {
        return page -> {
            if (roleComboBox.getValue() != null && domainComboBox.getValue() != null) {
                generateGrid(page);
            }
        };
    }
}
