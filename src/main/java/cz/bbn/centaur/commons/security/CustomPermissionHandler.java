package cz.bbn.cerberus.commons.security;

import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.custompermission.dto.CustomPermType;
import cz.bbn.cerberus.custompermission.dto.PermUserDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public class CustomPermissionHandler {

    private static CustomPermissionHandler handler = null;
    private final LinkedHashSet<CustomPermissionProvider> serviceSet;
    private final Set<SecondaryOwnerProvider> secondaryOwnerProviderSet;

    private Map<Long, Map<String, Map<String, Set<String>>>> userDomainEntityPermissionMap = new HashMap<>();
    private Map<String, Map<String, Map<String, Set<String>>>> roleDomainEntityPermissionMap = new HashMap<>();

    private CustomPermissionHandler() {
        serviceSet = new LinkedHashSet<>();
        secondaryOwnerProviderSet = new HashSet<>();
    }

    protected static CustomPermissionHandler getInstance() {
        if (handler == null) {
            handler = new CustomPermissionHandler();
        }
        return handler;
    }

    protected Map<String, List<String>> getObjectMap() {
        LinkedHashMap<String, List<String>> objectMap = new LinkedHashMap<>();
        for (CustomPermissionProvider service : serviceSet) {
            if (service.showInCustomPermissions()) {
                for (DomainEnum domain : service.getDomainSet()) {
                    objectMap.put(domain.getValue(), service.findAllId());
                }
            }
        }
        return objectMap;
    }

    protected void addToCustomPermissionHandler(CustomPermissionProvider service) {
        serviceSet.add(service);
    }

    public void addToSecondaryOwnerProviderSet(SecondaryOwnerProvider secondaryOwnerProvider) {
        secondaryOwnerProviderSet.add(secondaryOwnerProvider);
    }

    protected Set<String> getReadPermission(String objectName) {
        AppUser user = SecurityUtils.getCurrentUser();
        Map<String, List<String>> objectMap = getObjectMap();
        Set<String> entitySet = new HashSet<>(getUserReadPermission(objectName, user.getId(), objectMap));
        Map<String, Long> ownerMap = getEntityOwnerMap(objectName);
        for (Map.Entry<String, Long> entry : ownerMap.entrySet()) {
            if (Objects.equals(entry.getValue(), user.getId())) {
                entitySet.add(entry.getKey());
            }
        }
        for (String role : user.getActiveRoleSet()) {
            entitySet.addAll(getRoleReadPermission(objectName, role, objectMap));
        }
        return entitySet;
    }

    public boolean hasUserPermission(String objectName, String objectId, String permissionId, Long userId) {
        return hasUserPermission(objectName, objectId, permissionId, userId, true);
    }

    protected boolean hasPermission(String objectName, String objectId, String permissionId, String secondaryId) {
        AppUser user = SecurityUtils.getCurrentUser();
        if (!SecurityUtils.hasPermission(Permission.valueOfOrNotExists(permissionId))) {
            return false;
        }
        Map<String, Long> ownerMap = getEntityOwnerMap(objectName);
        if (Objects.equals(ownerMap.get(objectId), user.getId())) {
            return true;
        }
        if (secondaryId != null && getSecondaryUsersEntitySet(objectName, user.getId()).contains(secondaryId)) {
            return true;
        }
        if (hasUserPermission(objectName, objectId, permissionId, user.getId(), false)) {
            return true;
        }
        for (String roleId : user.getActiveRoleSet()) {
            if (hasRolePermission(objectName, objectId, permissionId, roleId)) {
                return true;
            }
        }
        return false;
    }

    protected Set<String> getPermissionByObjectName(String key) {
        return Permission.getCustomStringSetByObjectName(key);
    }

    protected Map<Long, Map<String, Map<String, Set<String>>>> getUserDomainEntityPermissionMap() {
        return userDomainEntityPermissionMap;
    }

    protected void setUserDomainEntityPermissionMap(Map<Long, Map<String, Map<String, Set<String>>>> map) {
        this.userDomainEntityPermissionMap = map;
    }

    protected Map<String, Map<String, Map<String, Set<String>>>> getRoleDomainEntityPermissionMap() {
        return roleDomainEntityPermissionMap;
    }

    protected void setRoleDomainEntityPermissionMap(Map<String, Map<String, Map<String, Set<String>>>> map) {
        this.roleDomainEntityPermissionMap = map;
    }

    protected Set<String> getAllowedEntityIdSet(Set<String> permission) {
        Map<String, Map<String, Long>> domainEntityOwnerMap = getDomainEntityOwnerMap();
        Set<String> allowedEntitySet = new HashSet<>();
        AppUser user = SecurityUtils.getCurrentUser();
        Long userId = user.getId();

        for (String domain : domainEntityOwnerMap.keySet()) {
            addToPermMap(domainEntityOwnerMap, allowedEntitySet, domain, userId);
        }

        if (userDomainEntityPermissionMap.containsKey(userId)) {
            for (String domain : userDomainEntityPermissionMap.get(userId).keySet()) {
                allowedEntitySet.addAll(getUserSpecificPermission(domain, permission, userId));
            }
        }

        for (String role : user.getActiveRoleSet()) {
            for (String domain : roleDomainEntityPermissionMap.get(role).keySet()) {
                allowedEntitySet.addAll(getRoleSpecificPermission(domain, permission, role));
            }
        }

        return allowedEntitySet;
    }

    protected Set<String> getAllowedEntityIdByDomain(Set<String> permission, String domain, AppUser user) {
        Map<String, Map<String, Long>> domainEntityOwnerMap = getDomainEntityOwnerMap();
        Set<String> allowedEntityByDomain = new HashSet<>();
        Long userId = user.getId();

        if (domainEntityOwnerMap.containsKey(domain)) {
            Map<String, Long> entityOwnerMap = domainEntityOwnerMap.get(domain);
            for (Map.Entry<String, Long> entry : entityOwnerMap.entrySet()) {
                if (Objects.equals(entry.getValue(), userId)) {
                    allowedEntityByDomain.add(entry.getKey());
                }
            }
        }

        if (userDomainEntityPermissionMap.containsKey(userId)
                && userDomainEntityPermissionMap.get(userId).containsKey(domain)) {
            allowedEntityByDomain.addAll(getUserSpecificPermission(domain, permission, userId));
        }
        for (String role : user.getActiveRoleSet()) {
            if (roleDomainEntityPermissionMap.get(role).containsKey(domain)) {
                allowedEntityByDomain.addAll(getRoleSpecificPermission(domain, permission, role));
            }
        }
        return allowedEntityByDomain;
    }

    protected boolean hasCustomReadAll(String domain) {
        AppUser appUser = SecurityUtils.getCurrentUser();
        String allPerm = CustomPermissionService.ALL_PERMISSION;
        Long userId = appUser.getId();
        if (userDomainEntityPermissionMap.containsKey(userId)
                && userDomainEntityPermissionMap.get(userId).containsKey(domain)
                && userDomainEntityPermissionMap.get(userId).get(domain).containsKey(allPerm)) {
            if (userDomainEntityPermissionMap.get(userId).get(domain).get(allPerm).contains(allPerm)
                    || !Collections.disjoint(userDomainEntityPermissionMap.get(userId).get(domain).get(allPerm),
                    Permission.getReadCustomPermissionMap().get(domain))) {
                return true;
            }
        }
        for (String roleId : appUser.getActiveRoleSet()) {
            if (roleDomainEntityPermissionMap.containsKey(roleId)
                    && roleDomainEntityPermissionMap.get(roleId).containsKey(domain)
                    && roleDomainEntityPermissionMap.get(roleId).get(domain).containsKey(allPerm)) {
                if (roleDomainEntityPermissionMap.get(roleId).get(domain).get(allPerm).contains(allPerm)
                        || !Collections.disjoint(roleDomainEntityPermissionMap.get(roleId).get(domain).get(allPerm),
                        Permission.getReadCustomPermissionMap().get(domain))) {
                    return true;
                }
            }
        }
        return false;
    }

    protected Set<String> getSecondaryOwnerSet(String domain) {
        Long userId = SecurityUtils.getCurrentUserId();
        return getSecondaryUsersEntitySet(domain, userId);
    }

    protected Map<CustomPermType, Set<PermUserDto>> getCustomUserPermMap(String objectName, String objectId,
                                                                         String permissionId, List<UserDto> userList) {
        Map<CustomPermType, Set<PermUserDto>> permUserMap = new HashMap<>();
        Map<String, Long> ownerMap = getEntityOwnerMap(objectName);
        for (UserDto userDto : userList) {
            if (Objects.equals(ownerMap.get(objectId), userDto.getId())) {
                addToPermOwnerMap(userDto, permUserMap, true, CustomPermType.PERMANENT_PERM);
            } else if (userDomainEntityPermissionMap.containsKey(userDto.getId())
                    && userDomainEntityPermissionMap.get(userDto.getId()).containsKey(objectName)) {
                processPermOwnerMap(userDto, objectName, permissionId, objectId, permUserMap);
            } else {
                addToPermOwnerMap(userDto, permUserMap, false, CustomPermType.NO_PERM);
            }
        }
        return permUserMap;
    }

    private void processPermOwnerMap(UserDto userDto, String objectName, String permissionId, String objectId,
                                     Map<CustomPermType, Set<PermUserDto>> permUserMap) {
        String allPerm = CustomPermissionService.ALL_PERMISSION;
        if (userDomainEntityPermissionMap.get(userDto.getId()).get(objectName).containsKey(allPerm)) {
            if (userDomainEntityPermissionMap.get(userDto.getId()).get(objectName).get(allPerm)
                    .contains(allPerm)
                    || userDomainEntityPermissionMap.get(userDto.getId()).get(objectName).get(allPerm)
                    .contains(permissionId)) {
                addToPermOwnerMap(userDto, permUserMap, true, CustomPermType.PERMANENT_PERM);
            } else {
                addToPermOwnerMap(userDto, permUserMap, false, CustomPermType.NO_PERM);
            }
        } else if (userDomainEntityPermissionMap.get(userDto.getId()).get(objectName).containsKey(objectId)) {
            if (userDomainEntityPermissionMap.get(userDto.getId()).get(objectName).get(objectId)
                    .contains(allPerm)) {
                addToPermOwnerMap(userDto, permUserMap, true, CustomPermType.PERMANENT_PERM);
            } else if (userDomainEntityPermissionMap.get(userDto.getId()).get(objectName).get(objectId)
                    .contains(permissionId)) {
                addToPermOwnerMap(userDto, permUserMap, false, CustomPermType.CHANGEABLE_PERM);
            } else {
                addToPermOwnerMap(userDto, permUserMap, false, CustomPermType.NO_PERM);
            }
        } else {
            addToPermOwnerMap(userDto, permUserMap, false, CustomPermType.NO_PERM);
        }
    }

    private void addToPermOwnerMap(UserDto userDto, Map<CustomPermType, Set<PermUserDto>> permUserMap,
                                   boolean permanent, CustomPermType type) {
        PermUserDto permUserDto = new PermUserDto();
        permUserDto.setId(userDto.getId());
        if (permanent) {
            permUserDto.setName(userDto.getName() + " (" + Transl.get("Unchangeable") + ")");
        } else {
            permUserDto.setName(userDto.getName());
        }
        permUserDto.setPermanent(permanent);
        Set<PermUserDto> permUserSet;
        if (permUserMap.containsKey(type)) {
            permUserSet = permUserMap.get(type);
        } else {
            permUserSet = new HashSet<>();
        }
        permUserSet.add(permUserDto);
        permUserMap.put(type, permUserSet);
    }

    private boolean hasUserPermission(String objectName, String objectId, String permissionId,
                                      Long userId, boolean checkOwners) {
        if (checkOwners) {
            Map<String, Long> ownerMap = getEntityOwnerMap(objectName);
            if (Objects.equals(ownerMap.get(objectId), userId)) {
                return true;
            }
        }
        if (userDomainEntityPermissionMap.containsKey(userId)
                && userDomainEntityPermissionMap.get(userId).containsKey(objectName)) {
            if (userDomainEntityPermissionMap.get(userId).get(objectName).containsKey(objectId)
                    && (userDomainEntityPermissionMap.get(userId).get(objectName).get(objectId).contains(permissionId)
                    || userDomainEntityPermissionMap.get(userId).get(objectName).get(objectId)
                    .contains(CustomPermissionService.ALL_PERMISSION))) {
                return true;
            }
            return userDomainEntityPermissionMap.get(userId).get(objectName)
                    .containsKey(CustomPermissionService.ALL_PERMISSION) &&
                    (userDomainEntityPermissionMap.get(userId).get(objectName)
                            .get(CustomPermissionService.ALL_PERMISSION).contains(permissionId)
                            || userDomainEntityPermissionMap.get(userId).get(objectName)
                            .get(CustomPermissionService.ALL_PERMISSION)
                            .contains(CustomPermissionService.ALL_PERMISSION));
        }
        return false;
    }

    private Set<String> getUserReadPermission(String objectName, Long userId, Map<String, List<String>> objectMap) {
        Set<String> entitySet = new HashSet<>();
        if (userDomainEntityPermissionMap.containsKey(userId)
                && userDomainEntityPermissionMap.get(userId).containsKey(objectName)) {
            Map<String, Map<String, Set<String>>> domainPermMap = userDomainEntityPermissionMap.get(userId);
            Set<String> domainReadPermission = Permission.getReadCustomPermissionMap().get(objectName);
            Map<String, Set<String>> entityMap = domainPermMap.get(objectName);
            for (Map.Entry<String, Set<String>> entityEntry : entityMap.entrySet()) {
                addToReadPermSet(entityEntry.getKey(), entityEntry.getValue(), entitySet,
                        domainReadPermission, objectMap.get(objectName));
            }
        }
        return entitySet;
    }

    private void addToPermMap(Map<String, Map<String, Long>> domainEntityOwnerMap, Set<String> allowedEntitySet,
                              String domain, Long userId) {
        Map<String, Long> entityOwnerMap = domainEntityOwnerMap.get(domain);
        for (Map.Entry<String, Long> entry : entityOwnerMap.entrySet()) {
            if (Objects.equals(entry.getValue(), userId)) {
                allowedEntitySet.add(entry.getKey());
            }
        }
    }

    private Set<String> getRoleReadPermission(String objectName, String roleId, Map<String, List<String>> objectMap) {
        Set<String> entitySet = new HashSet<>();
        if (roleDomainEntityPermissionMap.containsKey(roleId)
                && roleDomainEntityPermissionMap.get(roleId).containsKey(objectName)) {
            Map<String, Map<String, Set<String>>> domainPermMap = roleDomainEntityPermissionMap.get(roleId);
            Set<String> domainReadPermission = Permission.getReadCustomPermissionMap().get(objectName);
            Map<String, Set<String>> entityMap = domainPermMap.get(objectName);
            for (Map.Entry<String, Set<String>> entityEntry : entityMap.entrySet()) {
                addToReadPermSet(entityEntry.getKey(), entityEntry.getValue(), entitySet,
                        domainReadPermission, objectMap.get(objectName));
            }
        }
        return entitySet;
    }

    private void addToReadPermSet(String entityId, Set<String> permSet,
                                  Set<String> entitySet, Set<String> domainReadPermission, List<String> objectList) {
        if (CustomPermissionService.ALL_PERMISSION.equals(entityId)) {
            if (permSet != null && domainReadPermission != null
                    && (!Collections.disjoint(permSet, domainReadPermission)
                    || permSet.contains(CustomPermissionService.ALL_PERMISSION))) {
                entitySet.addAll(objectList);
            }
        } else {
            if (permSet != null && domainReadPermission != null
                    && (!Collections.disjoint(permSet, domainReadPermission)
                    || permSet.contains(CustomPermissionService.ALL_PERMISSION))) {
                entitySet.add(entityId);
            }
        }
    }

    private Set<String> getUserSpecificPermission(String objectName, Set<String> permissionSet, Long userId) {
        Set<String> entitySet = new HashSet<>();
        if (userDomainEntityPermissionMap.containsKey(userId)
                && userDomainEntityPermissionMap.get(userId).containsKey(objectName)) {
            Map<String, Map<String, Set<String>>> domainPermMap = userDomainEntityPermissionMap.get(userId);
            Map<String, Set<String>> entityMap = domainPermMap.get(objectName);
            for (Map.Entry<String, Set<String>> entityEntry : entityMap.entrySet()) {
                addToSpecificPermSet(
                        entityEntry.getKey(), permissionSet, entityEntry.getValue(), entitySet, objectName);
            }
        }
        return entitySet;
    }

    private Set<String> getRoleSpecificPermission(String objectName, Set<String> permission, String roleId) {
        Set<String> entitySet = new HashSet<>();
        if (roleDomainEntityPermissionMap.containsKey(roleId)
                && roleDomainEntityPermissionMap.get(roleId).containsKey(objectName)) {
            Map<String, Map<String, Set<String>>> domainPermMap = roleDomainEntityPermissionMap.get(roleId);
            Map<String, Set<String>> entityMap = domainPermMap.get(objectName);
            for (Map.Entry<String, Set<String>> entityEntry : entityMap.entrySet()) {
                addToSpecificPermSet(entityEntry.getKey(), permission, entityEntry.getValue(), entitySet, objectName);
            }
        }
        return entitySet;
    }

    private void addToSpecificPermSet(String entityId, Set<String> permissionSet, Set<String> permSet,
                                      Set<String> entitySet, String objectName) {
        if (CustomPermissionService.ALL_PERMISSION.equals(entityId)) {
            if (permSet != null && permissionSet != null && (!Collections.disjoint(permSet, permissionSet)
                    || permSet.contains(CustomPermissionService.ALL_PERMISSION))) {
                entitySet.addAll(getObjectMap().get(objectName));
            }
        }
        if (permSet != null && permissionSet != null && (!Collections.disjoint(permSet, permissionSet)
                || permSet.contains(CustomPermissionService.ALL_PERMISSION))) {
            entitySet.add(entityId);
        }
    }

    private boolean hasRolePermission(String objectName, String objectId, String permissionId, String roleId) {
        if (roleDomainEntityPermissionMap.containsKey(roleId)
                && roleDomainEntityPermissionMap.get(roleId).containsKey(objectName)) {
            if (roleDomainEntityPermissionMap.get(roleId).get(objectName).containsKey(objectId)
                    && (roleDomainEntityPermissionMap.get(roleId).get(objectName).get(objectId).contains(permissionId)
                    || roleDomainEntityPermissionMap.get(roleId).get(objectName).get(objectId)
                    .contains(CustomPermissionService.ALL_PERMISSION))) {
                return true;
            }
            return roleDomainEntityPermissionMap.get(roleId).get(objectName)
                    .containsKey(CustomPermissionService.ALL_PERMISSION) &&
                    (roleDomainEntityPermissionMap.get(roleId).get(objectName)
                            .get(CustomPermissionService.ALL_PERMISSION).contains(permissionId)
                            || roleDomainEntityPermissionMap.get(roleId).get(objectName)
                            .get(CustomPermissionService.ALL_PERMISSION)
                            .contains(CustomPermissionService.ALL_PERMISSION));
        }
        return false;
    }

    private Map<String, Map<String, Long>> getDomainEntityOwnerMap() {
        Map<String, Map<String, Long>> domainOwnerMap = new HashMap<>();
        for (CustomPermissionProvider service : serviceSet) {
            for (DomainEnum domain : service.getDomainSet()) {
                domainOwnerMap.put(domain.getValue(), service.getOwnerMap());
            }
        }
        return domainOwnerMap;
    }

    private Map<String, Long> getEntityOwnerMap(String domainName) {
        for (CustomPermissionProvider service : serviceSet) {
            for (DomainEnum domain : service.getDomainSet()) {
                if (domain.getValue().equals(domainName)) {
                    return service.getOwnerMap();
                }
            }
        }
        return new HashMap<>();
    }

    private Set<String> getSecondaryUsersEntitySet(String domainName, Long userId) {
        for (SecondaryOwnerProvider provider : secondaryOwnerProviderSet) {
            for (DomainEnum domain : provider.getDomainSet()) {
                if (domain.getValue().equals(domainName)) {
                    return provider.getUserEntityIdSet(userId);
                }
            }
        }
        return new HashSet<>();
    }
}
