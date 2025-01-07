package cz.bbn.cerberus.permissionmanagement.ui;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.permissionmanagement.PermissionManagementComponentOperations;
import cz.bbn.cerberus.permissionmanagement.ui.component.PermissionManagementTabsComponent;
import cz.bbn.cerberus.permissionmanagement.ui.component.tab.CustomRolePermissionTab;
import cz.bbn.cerberus.permissionmanagement.ui.component.tab.CustomUserPermissionTab;
import cz.bbn.cerberus.permissionmanagement.ui.component.tab.PermissionTab;
import cz.bbn.cerberus.role.RoleService;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.role.dto.RoleHasPermissionDto;
import cz.bbn.cerberus.rolecustompermission.RoleCustomPermissionService;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Route(value = PermissionManagementView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.CUSTOM_PERMISSION_VIEW)
@Slf4j
public class PermissionManagementView extends AppView {

    public static final String ROUTE = "permission-management-view";

    private final EntityNewComponentOperation entityNewComponentOperation;
    private final PermissionManagementComponentOperations componentOperations;
    private final RoleService roleService;
    private final CustomPermissionService customPermissionService;
    private final RoleCustomPermissionService roleCustomPermissionService;
    private final List<RoleDto> roleList;
    private final AppEnv appEnv;
    private final Map<String, Set<String>> customPermissionStringMap;
    private final Map<String, List<String>> objectMap;

    private PermissionTab permissionTab;
    private CustomRolePermissionTab customRolePermissionTab;
    private CustomUserPermissionTab customUserPermissionTab;

    public PermissionManagementView(EntityNewComponentOperation entityNewComponentOperation,
                                    PermissionManagementComponentOperations componentOperations,
                                    RoleService roleService, CustomPermissionService customPermissionService,
                                    RoleCustomPermissionService roleCustomPermissionService, AppEnv appEnv) {

        this.entityNewComponentOperation = entityNewComponentOperation;
        this.componentOperations = componentOperations;
        this.roleService = roleService;
        this.customPermissionService = customPermissionService;
        this.roleCustomPermissionService = roleCustomPermissionService;
        this.roleList = roleService.findAll();
        this.appEnv = appEnv;
        this.customPermissionStringMap = Permission.getCustomPermissionStringMap();
        this.objectMap = componentOperations.getObjectMap();
        init();
    }

    private void init() {
        String title = Transl.get("Permission management");
        List<TabEntry> tabEntryList = new ArrayList<>();

        permissionTab = new PermissionTab(Permission.getPermissionMap(), roleList);
        tabEntryList.add(new TabEntry(Transl.get("Permissions"), permissionTab));

        customRolePermissionTab = new CustomRolePermissionTab(Permission.getCustomPermissionMap(),
                DomainEnum.getPermissionDomainSet(), componentOperations.getRoleList(),
                objectMap, new HashMap<>(SecurityUtils.getRoleDomainEntityPermissionMap()));
        tabEntryList.add(new TabEntry(Transl.get("Role permissions"), customRolePermissionTab));

        customUserPermissionTab = new CustomUserPermissionTab(Permission.getCustomPermissionMap(),
                DomainEnum.getPermissionDomainSet(), componentOperations.getUserList(),
                objectMap, new HashMap<>(SecurityUtils.getUserDomainEntityPermissionMap()));
        tabEntryList.add(new TabEntry(Transl.get("User permissions"), customUserPermissionTab));

        Button saveButton = VaadinComponents.getSubmitButton();
        saveButton.addClickListener(e -> save());

        PermissionManagementTabsComponent permissionManagementTabsComponent =
                new PermissionManagementTabsComponent(title, tabEntryList, saveButton, entityNewComponentOperation);
        permissionManagementTabsComponent.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation));
        this.add(permissionManagementTabsComponent);
    }

    private void save() {
        Map<String, Set<RoleHasPermissionDto>> roleMap = permissionTab.getRoleValues();
        roleService.savePermissions(roleMap);

        customPermissionService.save(
                processItemPermMap(customUserPermissionTab.getUserPermMap(), customUserPermissionTab.getChangedMap()));
        customUserPermissionTab.resetChangedMap();

        roleCustomPermissionService.save(
                processItemPermMap(customRolePermissionTab.getRolePermMap(), customRolePermissionTab.getChangedMap()));
        customRolePermissionTab.resetChangedMap();

        SuccessNotification.showSavingSuccess(appEnv);
    }

    private Map<String, Map<String, Map<String, Set<String>>>> processItemPermMap(
            Map<String, Map<String, Map<String, Set<String>>>> itemDomainEntityPermissionMap,
            Map<String, Set<String>> changedMap) {
        Map<String, Map<String, Map<String, Set<String>>>> processedItemDomainEntityPermMap = new HashMap<>();

        for (Map.Entry<String, Set<String>> itemEntry : changedMap.entrySet()) {
            Map<String, Map<String, Set<String>>> domainMap = new HashMap<>();
            for (String domain : changedMap.get(itemEntry.getKey())) {
                domainMap.put(domain, new HashMap<>());
                Map<String, Set<String>> entityMap = new HashMap<>();
                if (itemDomainEntityPermissionMap.containsKey(itemEntry.getKey())
                        && itemDomainEntityPermissionMap.get(itemEntry.getKey()).containsKey(domain)) {
                    processEntitiesPermissions(itemEntry.getKey(), domain, entityMap, itemDomainEntityPermissionMap);
                }
                domainMap.put(domain, entityMap);
            }
            processedItemDomainEntityPermMap.put(itemEntry.getKey(), domainMap);
        }
        processColumns(processedItemDomainEntityPermMap);
        return processedItemDomainEntityPermMap;
    }

    private void processEntitiesPermissions(String itemId, String domain, Map<String, Set<String>> entityMap,
                                            Map<String, Map<String, Map<String, Set<String>>>>
                                                    itemDomainEntityPermissionMap) {
        Map<String, Set<String>> entityPermSet = clearEmptyEntities(
                itemDomainEntityPermissionMap.get(itemId).get(domain));
        Set<String> entitySet = entityPermSet.keySet();
        if (entitySet.containsAll(objectMap.get(domain))) {
            processPermissions(entityPermSet, domain, entityMap);
        } else {
            for (Map.Entry<String, Set<String>> entityEntry : entityPermSet.entrySet()) {
                if (entityEntry.getValue().containsAll(customPermissionStringMap.get(domain))) {
                    Set<String> perm = new HashSet<>();
                    perm.add(CustomPermissionService.ALL_PERMISSION);
                    entityMap.put(entityEntry.getKey(), perm);
                } else {
                    entityMap.put(entityEntry.getKey(), entityEntry.getValue());
                }
            }
        }
    }

    private void processPermissions(Map<String, Set<String>> entityPermSet, String domain,
                                    Map<String, Set<String>> entityMap) {
        boolean hasAll = true;
        for (Map.Entry<String, Set<String>> entityEntry : entityPermSet.entrySet()) {
            if (entityEntry.getValue().containsAll(customPermissionStringMap.get(domain))) {
                Set<String> perm = new HashSet<>();
                perm.add(CustomPermissionService.ALL_PERMISSION);
                entityMap.put(entityEntry.getKey(), perm);
            } else {
                entityMap.put(entityEntry.getKey(), entityEntry.getValue());
                hasAll = false;
            }
        }
        if (hasAll) {
            Set<String> perm = new HashSet<>();
            perm.add(CustomPermissionService.ALL_PERMISSION);
            entityMap.clear();
            entityMap.put(CustomPermissionService.ALL_PERMISSION, perm);
        }
    }

    private Map<String, Set<String>> clearEmptyEntities(Map<String, Set<String>> entityPermMap) {
        Map<String, Set<String>> processedEntityPermMap = new HashMap<>();
        for (Map.Entry<String, Set<String>> roleEntry : entityPermMap.entrySet()) {
            if (roleEntry.getValue() != null && !roleEntry.getValue().isEmpty()) {
                processedEntityPermMap.put(roleEntry.getKey(), roleEntry.getValue());
            }
        }
        return processedEntityPermMap;
    }

    private void processColumns(Map<String, Map<String, Map<String, Set<String>>>> processedItemDomainEntityPermMap) {
        for (Map.Entry<String, Map<String, Map<String, Set<String>>>> itemEntry
                : processedItemDomainEntityPermMap.entrySet()) {
            Map<String, Map<String, Set<String>>> domainMap = itemEntry.getValue();
            for (Map.Entry<String, Map<String, Set<String>>> domainEntry : domainMap.entrySet()) {
                Map<String, Set<String>> entityMap = domainEntry.getValue();
                if (domainEntry.getValue().keySet().containsAll(objectMap.get(domainEntry.getKey()))) {
                    processDomainColumn(domainEntry, entityMap, domainMap);
                }
            }
            processedItemDomainEntityPermMap.put(itemEntry.getKey(), domainMap);
        }
    }

    private void processDomainColumn(Map.Entry<String, Map<String, Set<String>>> domainEntry,
                                     Map<String, Set<String>> entityMap, Map<String,
            Map<String, Set<String>>> domainMap) {
        Map<String, Set<String>> permEntityMap = new HashMap<>();
        for (String permission : customPermissionStringMap.get(domainEntry.getKey())) {
            for (Map.Entry<String, Set<String>> entityEntry : entityMap.entrySet()) {
                addToPermEntitySet(entityEntry, permission, permEntityMap);
            }
        }
        Map<String, Set<String>> newEntityMap = new HashMap<>(entityMap);
        for (Map.Entry<String, Set<String>> permEntry : permEntityMap.entrySet()) {
            if (permEntry.getValue().containsAll(objectMap.get(domainEntry.getKey()))) {
                Set<String> currentPermSet;
                if (newEntityMap.containsKey(CustomPermissionService.ALL_PERMISSION)) {
                    currentPermSet = newEntityMap.get(CustomPermissionService.ALL_PERMISSION);
                } else {
                    currentPermSet = new HashSet<>();
                }
                currentPermSet.add(permEntry.getKey());
                newEntityMap.put(CustomPermissionService.ALL_PERMISSION, currentPermSet);
                processPermEntityEntry(entityMap, newEntityMap, permEntry);
            }
        }
        domainMap.put(domainEntry.getKey(), newEntityMap);
    }

    private void addToPermEntitySet(Map.Entry<String, Set<String>> entityEntry, String permission,
                                    Map<String, Set<String>> permEntityMap) {
        if (entityEntry.getValue().contains(permission)
                || entityEntry.getValue().contains(CustomPermissionService.ALL_PERMISSION)) {
            Set<String> entitySet;
            if (permEntityMap.containsKey(permission)) {
                entitySet = permEntityMap.get(permission);
            } else {
                entitySet = new HashSet<>();
            }
            entitySet.add(entityEntry.getKey());
            permEntityMap.put(permission, entitySet);
        }
    }

    private void processPermEntityEntry(Map<String, Set<String>> entityMap, Map<String, Set<String>> newEntityMap,
                                        Map.Entry<String, Set<String>> permEntry) {
        for (Map.Entry<String, Set<String>> entityEntry : entityMap.entrySet()) {
            Set<String> permSet;
            if (newEntityMap.containsKey(entityEntry.getKey())
                    && !CustomPermissionService.ALL_PERMISSION.equals(entityEntry.getKey())) {
                permSet = newEntityMap.get(entityEntry.getKey());
                permSet.remove(permEntry.getKey());
                if (permSet.isEmpty()) {
                    newEntityMap.remove(entityEntry.getKey());
                } else {
                    newEntityMap.put(entityEntry.getKey(), permSet);
                }
            }
        }
    }

}
