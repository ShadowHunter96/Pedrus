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
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static cz.bbn.cerberus.permissionmanagement.ui.component.CustomPermissionGridComponent.PAGE_SIZE;

public class CustomUserPermissionTab extends TabSimpleComponent {

    private final Map<DomainEnum, List<Permission>> customPermisionMap;
    private final Map<String, List<String>> objectMap;
    private final Set<DomainEnum> domainSet;
    private final List<UserDto> userList;

    private final ComboBox<DomainEnum> domainComboBox = new ComboBox<>(Transl.get("Domain"));
    private final ComboBox<UserDto> userComboBox = new ComboBox<>(Transl.get("User"));
    private final Checkbox ownedPermissions = new Checkbox(Transl.get("Owned permissions"));
    private final TextField search = new TextField(Transl.get("Search"));
    private final Label pageSizeLabel = new Label(Transl.get("Page size" + ": " + PAGE_SIZE));

    private Map<String, Map<String, Map<String, Set<String>>>> userDomainEntityPermissionMap;

    private CustomPermissionGridComponent customPermissionGridComponent;
    private PageChangeComponent pageChangeComponent;

    public CustomUserPermissionTab(Map<DomainEnum, List<Permission>> customPermissionMap,
                                   Set<DomainEnum> domainSet, List<UserDto> userList,
                                   Map<String, List<String>> objectMap,
                                   Map<Long, Map<String, Map<String, Set<String>>>> userDomainEntityPermissionMap) {
        this.customPermisionMap = customPermissionMap;
        this.domainSet = domainSet;
        this.userList = userList;
        this.objectMap = objectMap;
        this.userDomainEntityPermissionMap = getUserEntityPermissionMapWStr(userDomainEntityPermissionMap);
        initTab();
    }

    private Map<String, Map<String, Map<String, Set<String>>>> getUserEntityPermissionMapWStr(
            Map<Long, Map<String, Map<String, Set<String>>>> userEntityPermissionMap) {
        Map<String, Map<String, Map<String, Set<String>>>> userStrEntityPermMap = new HashMap<>();
        for (Map.Entry<Long, Map<String, Map<String, Set<String>>>> mapEntry : userEntityPermissionMap.entrySet()) {
            userStrEntityPermMap.put(String.valueOf(mapEntry.getKey()), mapEntry.getValue());
        }
        return userStrEntityPermMap;
    }

    private void initTab() {
        setSizeFull();
        getDomainsForCB();

        FormLayout formLayout = new FormLayout();
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);

        domainComboBox.setItems(getDomainsForCB());
        domainComboBox.setItemLabelGenerator(DomainEnum::getTranslatedName);
        userComboBox.setItems(userList);
        userComboBox.setItemLabelGenerator(UserDto::getLogin);
        ownedPermissions.setValue(true);

        formLayout.add(domainComboBox, userComboBox, ownedPermissions, search, pageSizeLabel);
        add(formLayout);

        customPermissionGridComponent = new CustomPermissionGridComponent(
                customPermisionMap, objectMap, domainComboBox, userDomainEntityPermissionMap, ownedPermissions);

        pageChangeComponent = new PageChangeComponent(getChangePageAction());

        domainComboBox.addValueChangeListener(e -> {
            if (e.getValue() != null && userComboBox.getValue() != null) {
                generateGrid(1);
            }
        });

        userComboBox.addValueChangeListener(e -> {
            if (domainComboBox.getValue() != null && e.getValue() != null) {
                generateGrid(1);
            }
        });
        ownedPermissions.addValueChangeListener(e -> {
            if (domainComboBox.getValue() != null && userComboBox.getValue() != null) {
                generateGrid(1);
            }
        });

        search.addValueChangeListener(e -> {
            if (domainComboBox.getValue() != null && userComboBox.getValue() != null) {
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
                    domainComboBox.getValue().getValue(), String.valueOf(userComboBox.getValue().getId()));
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
                String.valueOf(userComboBox.getValue().getId()),
                userDomainEntityPermissionMap, page, numberOfItems, items);
        pageChangeComponent.generateComponent(page, getNumberOfPages(numberOfItems));
    }

    private List<String> getEntitiesWithPermission(String domain, String role) {
        if (userDomainEntityPermissionMap.containsKey(role)
                && userDomainEntityPermissionMap.get(role).containsKey(domain)) {
            if (userDomainEntityPermissionMap.get(role).get(domain).containsKey(CustomPermissionService.ALL_PERMISSION)
                    && !userDomainEntityPermissionMap.get(role).get(domain).get(
                    CustomPermissionService.ALL_PERMISSION).isEmpty()) {
                List<String> toReturnList = objectMap.get(domain);
                Collections.sort(toReturnList);
                return toReturnList;
            }
            if (!userDomainEntityPermissionMap.get(role).get(domain).isEmpty()) {
                List<String> toReturnList = new ArrayList<>();
                Map<String, Set<String>> entityMap = userDomainEntityPermissionMap.get(role).get(domain);
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
        Set<DomainEnum> ownerMap = new LinkedHashSet<>();
        for (DomainEnum domain : domainSet) {
            if (objectMap.containsKey(domain.getValue())) {
                ownerMap.add(domain);
            }
        }
        return ownerMap;
    }

    private int getNumberOfPages(int numberOfItems) {
        int remainder = numberOfItems % PAGE_SIZE;
        int noOfPages = numberOfItems / PAGE_SIZE;
        if (remainder > 0) {
            noOfPages++;
        }
        return noOfPages;
    }

    public Map<String, Map<String, Map<String, Set<String>>>> getUserPermMap() {
        return userDomainEntityPermissionMap;
    }

    public Map<String, Set<String>> getChangedMap() {
        return customPermissionGridComponent.getItemDomainChanged();
    }

    public void resetChangedMap() {
        this.userDomainEntityPermissionMap =
                getUserEntityPermissionMapWStr(SecurityUtils.getUserDomainEntityPermissionMap());
        this.customPermissionGridComponent.resetChangedMap();
        if (domainComboBox.getValue() != null && userComboBox.getValue() != null) {
            generateGrid(1);
        }
    }

    public ChangePageAction getChangePageAction() {
        return page -> {
            if (userComboBox.getValue() != null && domainComboBox.getValue() != null) {
                generateGrid(page);
            }
        };
    }
}
