package cz.bbn.cerberus.permissionmanagement.ui.component;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class CustomPermissionGridComponent extends VerticalLayout {

    public static final int PAGE_SIZE = 50;

    private final Map<DomainEnum, List<Permission>> customPermisionMap;
    private final Map<String, List<String>> objectMap;
    private final Map<String, Set<String>> domainPermissionStringMap;
    private final ComboBox<DomainEnum> domainComboBox;
    private final Checkbox ownedPermissions;

    private final Map<String, Set<String>> itemDomainChanged = new HashMap<>();

    private final HorizontalLayout lineLayout = new HorizontalLayout();
    private final HorizontalLayout gridLayout = new HorizontalLayout();

    private Set<String> entityInitAllSet;
    private Set<String> permInitAllSet;
    private boolean initHasAll;

    private List<String> currentObjectList = new ArrayList<>();

    private Map<String, Map<String, Map<String, Set<String>>>> itemDomainEntityPermissionMap;
    private Map<String, Checkbox> entityAllCheckboxMap;
    private Map<String, Checkbox> permissionAllCheckboxMap;
    private Map<String, Map<String, Checkbox>> permissionEntityCheckboxMap;
    private Checkbox allPermissions;

    private int page;
    private int noOfItems;
    private List<String> items;

    private boolean ownedPermissionValue = false;

    public CustomPermissionGridComponent(Map<DomainEnum, List<Permission>> customPermisionMap,
                                         Map<String, List<String>> objectMap,
                                         ComboBox<DomainEnum> domainComboBox,
                                         Map<String, Map<String, Map<String, Set<String>>>>
                                                 itemDomainEntityPermissionMap, Checkbox ownedPermissions) {
        this.customPermisionMap = customPermisionMap;
        this.objectMap = objectMap;
        this.domainPermissionStringMap = generateDomainPermissionStringMap();
        this.domainComboBox = domainComboBox;
        this.itemDomainEntityPermissionMap = itemDomainEntityPermissionMap;
        this.ownedPermissions = ownedPermissions;
        init();
    }

    private void init() {
        setSizeFull();
        getElement().getStyle().set("overflow", "auto");
        lineLayout.setMargin(false);
        lineLayout.setPadding(false);
        lineLayout.setSpacing(false);
        lineLayout.setWidth("auto");
        lineLayout.getElement().getStyle().set("position", "sticky").set("top", "0")
                .set("background-color", "white").set("z-index", "1");

        gridLayout.setMargin(false);
        gridLayout.setPadding(false);
        gridLayout.setSpacing(false);
        gridLayout.setHeightFull();
        gridLayout.setWidth("auto");
        gridLayout.setAlignItems(Alignment.START);
        gridLayout.setDefaultVerticalComponentAlignment(Alignment.START);

        this.setMargin(false);
        this.setPadding(false);
        this.setSpacing(false);

        this.add(lineLayout, gridLayout);
    }

    public void generateGrid(DomainEnum domain, String itemId,
                             Map<String, Map<String, Map<String, Set<String>>>> itemDomainEntityPermissionMap,
                             int page, int noOfItems, List<String> items) {
        this.page = page;
        this.noOfItems = noOfItems;
        this.items = items;
        setCurrentObjectList(page, noOfItems, items);
        this.ownedPermissionValue = ownedPermissions.getValue();
        this.itemDomainEntityPermissionMap = itemDomainEntityPermissionMap;
        this.entityInitAllSet = new HashSet<>();
        this.permInitAllSet = new HashSet<>();
        this.initHasAll = false;
        generateAllPermission(itemId, domain.getValue());
        generateFirstLine(domain);
        gridLayout.removeAll();
        entityAllCheckboxMap = new HashMap<>();
        permissionAllCheckboxMap = new HashMap<>();
        permissionEntityCheckboxMap = new HashMap<>();
        generateFirstColumn(gridLayout, itemId);
        generateAllColumn(domain, gridLayout, itemId);

        for (Permission permission : customPermisionMap.get(domain)) {
            generatePermissionColumn(domain, gridLayout, permission.name(), itemId);
        }
        setAllPermissionsFromSets();
    }

    public Map<String, Set<String>> getItemDomainChanged() {
        return itemDomainChanged;
    }

    public void resetChangedMap() {
        this.itemDomainChanged.clear();
    }

    private void generateFirstLine(DomainEnum domain) {
        lineLayout.removeAll();
        Div div = new Div();
        div.setWidth("21em");
        div.setHeight("2em");
        Div emptyDiv = new Div();
        emptyDiv.addClassName("grid-block");
        div.add(emptyDiv);
        div.getElement().getStyle().set("position", "sticky").set("top", "0").set("left", "0")
                .set("background-color", "white").set("z-index", "1");
        lineLayout.add(div);

        VerticalLayout allLayout = new VerticalLayout();
        allLayout.setAlignItems(FlexComponent.Alignment.CENTER);
        allLayout.setHeight("2em");
        allLayout.setMargin(false);
        allLayout.setPadding(false);
        allLayout.setSpacing(false);
        allLayout.setWidth("15em");
        Div allDiv = new Div();
        allDiv.add(new Label(Transl.get("All")));
        allDiv.addClassNames("grid-block", "align-center-simple");
        allLayout.add(allDiv);
        lineLayout.add(allLayout);

        for (Permission permission : customPermisionMap.get(domain)) {
            VerticalLayout outerLayout = new VerticalLayout();
            outerLayout.setAlignItems(FlexComponent.Alignment.CENTER);
            outerLayout.setHeight("2em");
            outerLayout.setMargin(false);
            outerLayout.setPadding(false);
            outerLayout.setSpacing(false);
            outerLayout.setWidth("15em");
            Div permissionName = new Div();
            permissionName.add(new Label(Transl.get(permission.getTitle())));
            permissionName.addClassNames("grid-block", "align-center-simple");
            permissionName.getElement().setProperty(TextValues.TITLE, Transl.get(permission.getDescription()));

            outerLayout.add(permissionName);
            lineLayout.add(outerLayout);
        }
    }

    private void generateFirstColumn(HorizontalLayout gridLayout, String itemId) {

        VerticalLayout firstColumn = new VerticalLayout();
        firstColumn.setMargin(false);
        firstColumn.setPadding(false);
        firstColumn.setSpacing(false);
        firstColumn.setMinWidth("21em");
        firstColumn.getElement().getStyle().set("position", "sticky").set("left", "0")
                .set("background-color", "white").set("z-index", "1").set("margin-top", "calc(-1.5em - 10px)");

        Div emptyDiv = new Div();
        emptyDiv.setMinWidth("calc(21em + 10px)");
        emptyDiv.setMinHeight("calc(1.5em + 13px)");
        emptyDiv.getElement().getStyle().set("position", "sticky").set("top", "0").set("left", "0")
                .set("background-color", "white").set("margin-top", "-3px");
        firstColumn.add(emptyDiv);

        Div allDiv = new Div();
        allDiv.addClassName("grid-block");
        Label all = new Label(Transl.get("All"));
        allDiv.add(all);
        firstColumn.add(allDiv);
        allDiv.setVisible(!ownedPermissionValue);

        String domainStr = domainComboBox.getValue().getValue();

        for (String entityId : currentObjectList) {
            Div itemDiv = new Div();
            itemDiv.addClassName("grid-block");
            Label item = new Label(entityId);
            itemDiv.add(item);
            firstColumn.add(itemDiv);
            checkIfContainsOwned(itemId, domainStr, entityId, itemDiv);
        }

        gridLayout.add(firstColumn);
    }

    private void generateAllColumn(DomainEnum domain, HorizontalLayout gridLayout, String itemId) {

        VerticalLayout column = new VerticalLayout();
        column.setAlignItems(FlexComponent.Alignment.CENTER);
        column.setHeightFull();
        column.setMargin(false);
        column.setPadding(false);
        column.setSpacing(false);
        column.setMinWidth("15em");

        Div allDiv = new Div();
        allDiv.addClassNames("grid-block", "align-center-simple");
        allPermissions = new Checkbox();
        allPermissions.addValueChangeListener(e -> {
            if (e.isFromClient()) {
                setAllAllPermission(itemId, domain.getValue(), e.getValue());
                checkAllAll(allPermissions.getValue());
                addToItemDomainChangedMap(itemId, domain.getValue());
            }
        });
        setAllPermissionCheckbox(itemId, domain.getValue(), allPermissions);
        allDiv.add(allPermissions);
        allDiv.add(generateClearSpan(itemId, domain, null, null));
        column.add(allDiv);
        allDiv.setVisible(!ownedPermissionValue);

        for (String entityId : currentObjectList) {
            Div itemDiv = new Div();
            itemDiv.addClassNames("grid-block", "align-center-simple");
            Checkbox item = new Checkbox();
            entityAllCheckboxMap.put(entityId, item);
            item.addValueChangeListener(e -> {
                if (e.isFromClient()) {
                    setAllLinePermission(itemId, domain.getValue(), entityId, e.getValue());
                    checkAllLine(e.getValue(), entityId, true);
                    addToItemDomainChangedMap(itemId, domain.getValue());
                }
            });
            setAllPermissionLineCheckbox(itemId, domain.getValue(), entityId, item);
            itemDiv.add(item);
            itemDiv.add(generateClearSpan(itemId, domain, entityId, null));
            column.add(itemDiv);
            checkIfContainsOwned(itemId, domain.getValue(), entityId, itemDiv);
        }
        gridLayout.add(column);
    }

    private void generatePermissionColumn(DomainEnum domain, HorizontalLayout gridLayout,
                                          String permission, String itemId) {

        VerticalLayout column = new VerticalLayout();
        column.setAlignItems(FlexComponent.Alignment.CENTER);
        column.setHeightFull();
        column.setMargin(false);
        column.setPadding(false);
        column.setSpacing(false);
        column.setMinWidth("15em");

        Div allDiv = new Div();
        allDiv.addClassNames("grid-block", "align-center-simple");
        Checkbox all = new Checkbox();
        all.addValueChangeListener(e -> {
            if (e.isFromClient()) {
                setAllColumnPermission(itemId, domain.getValue(), permission, e.getValue());
                checkAllColumn(e.getValue(), permission, true);
                addToItemDomainChangedMap(itemId, domain.getValue());
            }
        });
        allDiv.add(all);
        allDiv.add(generateClearSpan(itemId, domain, null, permission));
        column.add(allDiv);
        permissionAllCheckboxMap.put(permission, all);
        setAllPermissionColumnCheckbox(itemId, domain.getValue(), permission, all);
        allDiv.setVisible(!ownedPermissionValue);

        Map<String, Checkbox> checkboxMap = new HashMap<>();

        for (String entityId : currentObjectList) {
            Div itemDiv = new Div();
            itemDiv.addClassNames("grid-block", "align-center-simple");
            Checkbox item = new Checkbox();
            if (itemDomainEntityPermissionMap.containsKey(itemId)
                    && itemDomainEntityPermissionMap.get(itemId).containsKey(domain.getValue())
                    && itemDomainEntityPermissionMap.get(itemId).get(domain.getValue()).containsKey(entityId)) {
                item.setValue(itemDomainEntityPermissionMap.get(itemId).get(domain.getValue())
                        .get(entityId).contains(permission));
            } else {
                item.setValue(false);
            }
            item.addValueChangeListener(e -> {
                if (e.isFromClient()) {
                    changePermissionValue(
                            itemId, domainComboBox.getValue().getValue(), entityId, permission, e.getValue());
                    addToItemDomainChangedMap(itemId, domain.getValue());
                }
            });
            checkboxMap.put(entityId, item);
            itemDiv.add(item);
            column.add(itemDiv);
            checkIfContainsOwned(itemId, domain.getValue(), entityId, itemDiv);
        }
        permissionEntityCheckboxMap.put(permission, checkboxMap);
        gridLayout.add(column);

    }

    private void changePermissionValue(String itemId, String domain, String entityId, String permission,
                                       boolean value) {
        Map<String, Set<String>> entityPermissionMap;
        Set<String> permissionSet;
        if (itemDomainEntityPermissionMap.containsKey(itemId)
                && itemDomainEntityPermissionMap.get(itemId).containsKey(domain)) {
            entityPermissionMap = itemDomainEntityPermissionMap.get(itemId).get(domain);
        } else {
            entityPermissionMap = new HashMap<>();
        }
        if (entityPermissionMap.containsKey(entityId)) {
            permissionSet = entityPermissionMap.get(entityId);
        } else {
            permissionSet = new HashSet<>();
        }
        if (value) {
            permissionSet.add(permission);
        } else {
            permissionSet.remove(permission);
        }
        entityPermissionMap.put(entityId, permissionSet);
        Map<String, Map<String, Set<String>>> domainEntityPermMap;
        if (itemDomainEntityPermissionMap.containsKey(itemId)) {
            domainEntityPermMap = itemDomainEntityPermissionMap.get(itemId);
        } else {
            domainEntityPermMap = new HashMap<>();
        }
        domainEntityPermMap.put(domain, entityPermissionMap);
        itemDomainEntityPermissionMap.put(itemId, domainEntityPermMap);
    }

    private void addToItemDomainChangedMap(String itemId, String domain) {
        Set<String> domainSet;
        if (itemDomainChanged.containsKey(itemId)) {
            domainSet = itemDomainChanged.get(itemId);
        } else {
            domainSet = new HashSet<>();
        }
        domainSet.add(domain);
        itemDomainChanged.put(itemId, domainSet);
    }

    private Map<String, Set<String>> generateDomainPermissionStringMap() {
        Map<String, Set<String>> domainPermission = new HashMap<>();
        for (Map.Entry<DomainEnum, List<Permission>> domainEntry : customPermisionMap.entrySet()) {
            Set<String> permissionNameSet = new HashSet<>();
            for (Permission permission : customPermisionMap.get(domainEntry.getKey())) {
                permissionNameSet.add(permission.name());
            }
            domainPermission.put(domainEntry.getKey().getValue(), permissionNameSet);
        }
        return domainPermission;
    }

    private void generateAllPermission(String itemId, String domain) {
        if (itemDomainEntityPermissionMap.containsKey(itemId)
                && itemDomainEntityPermissionMap.get(itemId).containsKey(domain)) {
            Map<String, Set<String>> entityMap = new HashMap<>();
            if (itemDomainEntityPermissionMap.get(itemId).get(domain)
                    .containsKey(CustomPermissionService.ALL_PERMISSION)) {
                fillAllEntityPermMap(itemId, domain, entityMap);
            }
            fillEntityPermMap(itemId, domain, entityMap);
            Map<String, Map<String, Set<String>>> domainMap = itemDomainEntityPermissionMap.get(itemId);
            domainMap.put(domain, entityMap);
            itemDomainEntityPermissionMap.put(itemId, domainMap);
        }
    }

    private void fillAllEntityPermMap(String itemId, String domain, Map<String, Set<String>> entityMap) {
        Set<String> permSet = new HashSet<>();
        if (itemDomainEntityPermissionMap.get(itemId).get(domain)
                .get(CustomPermissionService.ALL_PERMISSION).contains(CustomPermissionService.ALL_PERMISSION)) {
            permSet.addAll(domainPermissionStringMap.get(domain));
            permSet.add(CustomPermissionService.ALL_PERMISSION);
        } else {
            permSet.addAll(itemDomainEntityPermissionMap.get(itemId).get(domain)
                    .get(CustomPermissionService.ALL_PERMISSION));
        }
        for (String entity : objectMap.get(domain)) {
            Set<String> curPermSet;
            if (entityMap.containsKey(entity)) {
                curPermSet = entityMap.get(entity);
            } else {
                curPermSet = new HashSet<>();
            }
            curPermSet.addAll(permSet);
            entityMap.put(entity, curPermSet);
        }
    }

    private void fillEntityPermMap(String itemId, String domain, Map<String, Set<String>> entityMap) {
        for (Map.Entry<String, Set<String>> entityEntry
                : itemDomainEntityPermissionMap.get(itemId).get(domain).entrySet()) {
            Set<String> permSet;
            if (entityMap.containsKey(entityEntry.getKey())) {
                permSet = entityMap.get(entityEntry.getKey());
            } else {
                permSet = new HashSet<>();
            }
            if (entityEntry.getValue().contains(CustomPermissionService.ALL_PERMISSION)) {
                permSet.addAll(domainPermissionStringMap.get(domain));
                permSet.add(CustomPermissionService.ALL_PERMISSION);
            } else {
                permSet.addAll(entityEntry.getValue());
            }
            entityMap.put(entityEntry.getKey(), permSet);
        }
    }

    private void checkIfContainsOwned(String itemId, String domain, String entityId, Div itemDiv) {
        if (ownedPermissionValue && !(itemDomainEntityPermissionMap.containsKey(itemId)
                && itemDomainEntityPermissionMap.get(itemId).containsKey(domain)
                && itemDomainEntityPermissionMap.get(itemId).get(domain).containsKey(entityId))) {
            itemDiv.setVisible(false);
        }
    }

    private void checkAllAll(boolean value) {
        Set<String> entitySet = new HashSet<>();
        Set<String> permissionSet = new HashSet<>();
        for (Map.Entry<String, Checkbox> entry : entityAllCheckboxMap.entrySet()) {
            entry.getValue().setEnabled(!value);
            if (Boolean.TRUE.equals(entry.getValue().getValue())) {
                entitySet.add(entry.getKey());
            }
        }

        for (Map.Entry<String, Checkbox> entry : permissionAllCheckboxMap.entrySet()) {
            entry.getValue().setEnabled(!value);
            if (Boolean.TRUE.equals(entry.getValue().getValue())) {
                permissionSet.add(entry.getKey());
            }
        }

        for (Map.Entry<String, Map<String, Checkbox>> firstEntry : permissionEntityCheckboxMap.entrySet()) {
            if (!permissionSet.contains(firstEntry.getKey())) {
                for (Map.Entry<String, Checkbox> secondEntry : firstEntry.getValue().entrySet()) {
                    if (!entitySet.contains(secondEntry.getKey())) {
                        secondEntry.getValue().setEnabled(!value);
                    }
                }
            }
        }
    }

    private void checkAllLine(boolean value, String entityId, boolean checkItems) {
        Set<String> permissionSet = new HashSet<>();
        if (checkItems) {
            for (Map.Entry<String, Checkbox> entry : permissionAllCheckboxMap.entrySet()) {
                if (Boolean.TRUE.equals(entry.getValue().getValue())) {
                    permissionSet.add(entry.getKey());
                }
            }
        }
        for (Map.Entry<String, Map<String, Checkbox>> firstEntry : permissionEntityCheckboxMap.entrySet()) {
            if (!permissionSet.contains(firstEntry.getKey())) {
                for (Map.Entry<String, Checkbox> secondEntry : firstEntry.getValue().entrySet()) {
                    if (secondEntry.getKey().equals(entityId)) {
                        secondEntry.getValue().setEnabled(!value);
                    }
                }
            }
        }
    }

    private void checkAllColumn(boolean value, String permissionId, boolean checkItems) {
        Set<String> entitySet = new HashSet<>();
        if (checkItems) {
            for (Map.Entry<String, Checkbox> entry : entityAllCheckboxMap.entrySet()) {
                if (Boolean.TRUE.equals(entry.getValue().getValue())) {
                    entitySet.add(entry.getKey());
                }
            }
        }
        for (Map.Entry<String, Map<String, Checkbox>> firstEntry : permissionEntityCheckboxMap.entrySet()) {
            if (firstEntry.getKey().equals(permissionId)) {
                for (Map.Entry<String, Checkbox> secondEntry : firstEntry.getValue().entrySet()) {
                    if (!entitySet.contains(secondEntry.getKey())) {
                        secondEntry.getValue().setEnabled(!value);
                    }
                }
            }
        }
    }

    private void setAllAllPermission(String itemId, String domain, boolean value) {
        Map<String, Map<String, Set<String>>> itemMap = new HashMap<>();
        Map<String, Set<String>> domainMap = new HashMap<>();
        Set<String> permSet = new HashSet<>();
        if (itemDomainEntityPermissionMap.containsKey(itemId)) {
            itemMap = itemDomainEntityPermissionMap.get(itemId);
            if (itemDomainEntityPermissionMap.get(itemId).containsKey(domain)) {
                domainMap = itemDomainEntityPermissionMap.get(itemId).get(domain);
                if (domainMap.containsKey(CustomPermissionService.ALL_PERMISSION)) {
                    permSet = domainMap.get(CustomPermissionService.ALL_PERMISSION);
                }
            }
        }
        if (Boolean.TRUE.equals(value)) {
            permSet.add(CustomPermissionService.ALL_PERMISSION);
        } else {
            permSet.remove(CustomPermissionService.ALL_PERMISSION);
        }
        domainMap.put(CustomPermissionService.ALL_PERMISSION, permSet);
        itemMap.put(domain, domainMap);
        itemDomainEntityPermissionMap.put(itemId, itemMap);
    }

    private void setAllLinePermission(String itemId, String domain, String entityId, boolean value) {
        Map<String, Map<String, Set<String>>> itemMap = new HashMap<>();
        Map<String, Set<String>> domainMap = new HashMap<>();
        Set<String> permSet = new HashSet<>();
        if (itemDomainEntityPermissionMap.containsKey(itemId)) {
            itemMap = itemDomainEntityPermissionMap.get(itemId);
            if (itemDomainEntityPermissionMap.get(itemId).containsKey(domain)) {
                domainMap = itemDomainEntityPermissionMap.get(itemId).get(domain);
                if (domainMap.containsKey(entityId)) {
                    permSet = domainMap.get(entityId);
                }
            }
        }
        if (Boolean.TRUE.equals(value)) {
            permSet.add(CustomPermissionService.ALL_PERMISSION);
        } else {
            permSet.remove(CustomPermissionService.ALL_PERMISSION);
        }
        domainMap.put(entityId, permSet);
        itemMap.put(domain, domainMap);
        itemDomainEntityPermissionMap.put(itemId, itemMap);
    }

    private void setAllColumnPermission(String itemId, String domain, String permissionId, boolean value) {
        Map<String, Map<String, Set<String>>> itemMap = new HashMap<>();
        Map<String, Set<String>> domainMap = new HashMap<>();
        Set<String> permSet = new HashSet<>();
        if (itemDomainEntityPermissionMap.containsKey(itemId)) {
            itemMap = itemDomainEntityPermissionMap.get(itemId);
            if (itemDomainEntityPermissionMap.get(itemId).containsKey(domain)) {
                domainMap = itemDomainEntityPermissionMap.get(itemId).get(domain);
                if (domainMap.containsKey(CustomPermissionService.ALL_PERMISSION)) {
                    permSet = domainMap.get(CustomPermissionService.ALL_PERMISSION);
                }
            }
        }
        if (Boolean.TRUE.equals(value)) {
            permSet.add(permissionId);
        } else {
            permSet.remove(permissionId);
        }
        domainMap.put(CustomPermissionService.ALL_PERMISSION, permSet);
        itemMap.put(domain, domainMap);
        itemDomainEntityPermissionMap.put(itemId, itemMap);
    }

    private void setAllPermissionCheckbox(String itemId, String domain, Checkbox checkbox) {
        if (itemDomainEntityPermissionMap.containsKey(itemId)
                && itemDomainEntityPermissionMap.get(itemId).containsKey(domain)
                && itemDomainEntityPermissionMap.get(itemId).get(domain).containsKey(
                CustomPermissionService.ALL_PERMISSION)
                && itemDomainEntityPermissionMap.get(itemId).get(domain).get(
                CustomPermissionService.ALL_PERMISSION).contains(CustomPermissionService.ALL_PERMISSION)) {
            checkbox.setValue(true);
            initHasAll = true;
        }
    }

    private void setAllPermissionLineCheckbox(String itemId, String domain, String entityId, Checkbox checkbox) {
        if (itemDomainEntityPermissionMap.containsKey(itemId)
                && itemDomainEntityPermissionMap.get(itemId).containsKey(domain)
                && itemDomainEntityPermissionMap.get(itemId).get(domain).containsKey(entityId)
                && itemDomainEntityPermissionMap.get(itemId).get(domain).get(entityId)
                .contains(CustomPermissionService.ALL_PERMISSION)) {
            checkbox.setValue(true);
            entityInitAllSet.add(entityId);
        }
    }

    private void setAllPermissionColumnCheckbox(String itemId, String domain, String permissionId, Checkbox checkbox) {
        if (itemDomainEntityPermissionMap.containsKey(itemId)
                && itemDomainEntityPermissionMap.get(itemId).containsKey(domain)
                && itemDomainEntityPermissionMap.get(itemId).get(domain)
                .containsKey(CustomPermissionService.ALL_PERMISSION)
                && itemDomainEntityPermissionMap.get(itemId).get(domain).get(CustomPermissionService.ALL_PERMISSION)
                .contains(permissionId)) {
            checkbox.setValue(true);
            permInitAllSet.add(permissionId);
        }
    }

    private void setAllPermissionsFromSets() {
        for (String entityId : entityInitAllSet) {
            checkAllLine(true, entityId, false);
        }
        for (String permId : permInitAllSet) {
            checkAllColumn(true, permId, false);
        }
        if (initHasAll) {
            checkAllAll(true);
        }

    }

    private void setCurrentObjectList(int page, int noOfItems, List<String> items) {
        if (noOfItems > 0 && page > 0) {
            int end = (page * PAGE_SIZE) - 1;
            int start = end - PAGE_SIZE + 1;

            if (noOfItems > end) {
                currentObjectList = items.subList(start, end);
            } else {
                currentObjectList = items.subList(start, items.size());
            }
        } else {
            currentObjectList = new ArrayList<>();
        }
    }

    private Span generateClearSpan(String itemId, DomainEnum domain, String entityId, String permissionId) {
        Span span = new Span("X");
        span.getElement().getStyle().set("color", "red").set("margin-left", "0.5em");
        span.addClassName("cursor-pointer");
        span.addClickListener(e -> clearPermission(itemId, domain, entityId, permissionId));
        return span;
    }

    private void clearPermission(String itemId, DomainEnum domain, String entityId, String permissionId) {
        if (entityId == null && permissionId == null) {
            if (itemDomainEntityPermissionMap.containsKey(itemId)
                    && itemDomainEntityPermissionMap.get(itemId).containsKey(domain.getValue())) {
                Map<String, Map<String, Set<String>>> domainMap = itemDomainEntityPermissionMap.get(itemId);
                domainMap.put(domain.getValue(), new HashMap<>());
                itemDomainEntityPermissionMap.put(itemId, domainMap);
            }
            generateGrid(domain, itemId, itemDomainEntityPermissionMap, page, noOfItems, items);
            return;
        }
        if (entityId != null && permissionId == null) {
            Map<String, Map<String, Set<String>>> domainMap = itemDomainEntityPermissionMap.get(itemId);
            Map<String, Set<String>> entityMap = domainMap.get(domain.getValue());
            entityMap.put(entityId, new HashSet<>());
            domainMap.put(domain.getValue(), entityMap);
            itemDomainEntityPermissionMap.put(itemId, domainMap);
            generateGrid(domain, itemId, itemDomainEntityPermissionMap, page, noOfItems, items);
            return;
        }
        if (entityId == null) {
            Map<String, Map<String, Set<String>>> domainMap = itemDomainEntityPermissionMap.get(itemId);
            Map<String, Set<String>> entityMap = domainMap.get(domain.getValue());
            Map<String, Set<String>> newEntityMap = new HashMap<>();
            for (Map.Entry<String, Set<String>> entry : entityMap.entrySet()) {
                Set<String> permSet = entry.getValue();
                permSet.remove(permissionId);
                newEntityMap.put(entry.getKey(), permSet);
            }
            domainMap.put(domain.getValue(), newEntityMap);
            itemDomainEntityPermissionMap.put(itemId, domainMap);
            generateGrid(domain, itemId, itemDomainEntityPermissionMap, page, noOfItems, items);
        }
    }
}
