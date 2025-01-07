package cz.bbn.cerberus.custompermission;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactperson.dto.ContactPersonObjectTypeEnum;
import cz.bbn.cerberus.contactperson.persistance.repository.ContactPersonByObjectRepository;
import cz.bbn.cerberus.custompermission.dto.CustomUserPermissionDto;
import cz.bbn.cerberus.custompermission.factory.CustomUserPermissionFactory;
import cz.bbn.cerberus.custompermission.persistance.CustomUserPermissionEntity;
import cz.bbn.cerberus.custompermission.persistance.CustomUserPermissionRepository;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.dto.UserDto;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

@Service
public class CustomPermissionService {

    public static final String ALL_PERMISSION = "-- ALL --";
    public static final String CUSTOM_PERMISSION_OBJECT_NAME = "Custom permission";

    private final CustomUserPermissionRepository customUserPermissionRepository;
    private final AppLogService appLogService;
    private final ContactPersonByObjectRepository contactPersonByObjectRepository;
    private final UserService userService;

    public CustomPermissionService(CustomUserPermissionRepository customUserPermissionRepository,
                                   AppLogService appLogService,
                                   ContactPersonByObjectRepository contactPersonByObjectRepository,
                                   UserService userService) {
        this.customUserPermissionRepository = customUserPermissionRepository;
        this.appLogService = appLogService;
        this.contactPersonByObjectRepository = contactPersonByObjectRepository;
        this.userService = userService;
        loadPermissionSetForAllUsers();
    }

    public Set<CustomUserPermissionDto> getPermissionByObjectAndUser(String object, Long userId, String objectId) {
        return ConvertEntities.fromEntities(customUserPermissionRepository.getPermissionByObjectAndUser(
                object, userId, objectId), CustomUserPermissionFactory::fromEntity);
    }

    @Transactional
    public void save(Map<String, Map<String, Map<String, Set<String>>>> userDomainEntityPermMap) {
        Set<CustomUserPermissionEntity> allEntitySet = new HashSet<>();
        for (Map.Entry<String, Map<String, Map<String, Set<String>>>> userEntry : userDomainEntityPermMap.entrySet()) {
            Long userId = Long.valueOf(userEntry.getKey());
            for (String domain : userDomainEntityPermMap.get(userEntry.getKey()).keySet()) {
                addPermissionToSet(userId, userEntry.getKey(), domain, userDomainEntityPermMap, allEntitySet);
            }
        }
        customUserPermissionRepository.saveAll(allEntitySet);
        appLogService.logInsertMultiple(allEntitySet, CUSTOM_PERMISSION_OBJECT_NAME);
        for (String userId : userDomainEntityPermMap.keySet()) {
            loadPermissionSet(Long.valueOf(userId));
        }
    }

    @Transactional
    public void saveByUser(Set<CustomUserPermissionDto> dtoSet, String objectName,
                           String objectId, String permissionId) {
        Set<CustomUserPermissionEntity> existingPermissionSet = customUserPermissionRepository
                .getPermissionByObjectNameObjectIdPermissionId(objectName, objectId, permissionId);
        Set<Long> userIdSet = new HashSet<>();

        Set<CustomUserPermissionEntity> toSave = getToSaveSet(dtoSet, existingPermissionSet, userIdSet);
        Set<CustomUserPermissionEntity> toDelete = getToDeleteSet(existingPermissionSet, dtoSet, userIdSet);

        customUserPermissionRepository.saveAll(toSave);
        customUserPermissionRepository.deleteAll(toDelete);
        appLogService.logInsertMultiple(toSave, CUSTOM_PERMISSION_OBJECT_NAME);
        appLogService.logDeleteMultiple(toDelete, CUSTOM_PERMISSION_OBJECT_NAME);

        for (Long userId : userIdSet) {
            loadPermissionSetByUser(userId);
        }
    }

    private Set<CustomUserPermissionEntity> getToSaveSet(Set<CustomUserPermissionDto> dtoSet,
                                                         Set<CustomUserPermissionEntity> existingPermissionSet,
                                                         Set<Long> usernameSet) {
        Set<CustomUserPermissionEntity> toSave = new HashSet<>();
        for (CustomUserPermissionDto permissionDto : dtoSet) {
            boolean contains = false;
            for (CustomUserPermissionEntity permissionEntity : existingPermissionSet) {
                if (Objects.equals(String.valueOf(permissionDto.getUserId()), permissionEntity.getPermissionId())) {
                    contains = true;
                    break;
                }
            }
            if (!contains) {
                CustomUserPermissionEntity entity = new CustomUserPermissionEntity();
                CustomUserPermissionFactory.fillEntity(entity, permissionDto);
                usernameSet.add(entity.getUserId());
                toSave.add(entity);
            }
        }
        return toSave;
    }

    private Set<CustomUserPermissionEntity> getToDeleteSet(Set<CustomUserPermissionEntity> existingPermissionSet,
                                                           Set<CustomUserPermissionDto> dtoSet, Set<Long> usernameSet) {
        Set<CustomUserPermissionEntity> toDelete = new HashSet<>();
        for (CustomUserPermissionEntity permissionEntity : existingPermissionSet) {
            boolean contains = false;
            for (CustomUserPermissionDto permissionDto : dtoSet) {
                if (Objects.equals(permissionEntity.getUserId(), permissionDto.getUserId())) {
                    contains = true;
                    break;
                }
            }
            if (!contains) {
                usernameSet.add(permissionEntity.getUserId());
                toDelete.add(permissionEntity);
            }
        }
        return toDelete;
    }

    @Transactional
    public void saveSinglePermission(CustomUserPermissionDto dto) {
        CustomUserPermissionEntity entity = new CustomUserPermissionEntity();
        CustomUserPermissionFactory.fillEntity(entity, dto);
        customUserPermissionRepository.save(entity);
        appLogService.logInsert(dto, CUSTOM_PERMISSION_OBJECT_NAME);
        loadPermissionSetByUser(dto.getUserId());
    }

    public Set<Long> getUserNameSetByObjectIdObjectType(String id, String type) {
        return customUserPermissionRepository.getUserNameSetByObjectIdObjectType(id, type);
    }

    public void loadPermissionSetByCurrentUser() {
        loadPermissionSet(SecurityUtils.getCurrentUserId());
    }

    public void loadPermissionSetForAllUsers() {
        for (UserDto user : userService.findUserList()) {
            loadPermissionSetByUser(user.getId());
        }
    }

    public void loadPermissionSetByUser(Long userId) {
        loadPermissionSet(userId);
    }

    private void loadPermissionSet(Long userId) {
        Set<CustomUserPermissionDto> permissionDtoSet = ConvertEntities.fromEntities(
                customUserPermissionRepository.getByUserId(userId),
                CustomUserPermissionFactory::fromEntity);

        Map<String, Map<String, Set<String>>> domainEntityPermissionMap = new HashMap<>();

        for (CustomUserPermissionDto permissionDto : permissionDtoSet) {
            Map<String, Set<String>> entityPermissionMap;
            if (domainEntityPermissionMap.containsKey(permissionDto.getObjectName())) {
                entityPermissionMap = domainEntityPermissionMap.get(permissionDto.getObjectName());
            } else {
                entityPermissionMap = new HashMap<>();
            }
            Set<String> permissionSet;
            if (entityPermissionMap.containsKey(permissionDto.getObjectId())) {
                permissionSet = entityPermissionMap.get(permissionDto.getObjectId());
            } else {
                permissionSet = new HashSet<>();
            }
            permissionSet.add(permissionDto.getPermissionId());
            entityPermissionMap.put(permissionDto.getObjectId(), permissionSet);
            domainEntityPermissionMap.put(permissionDto.getObjectName(), entityPermissionMap);
        }

        Map<Long, Map<String, Map<String, Set<String>>>> map = SecurityUtils.getUserDomainEntityPermissionMap();
        map.put(userId, domainEntityPermissionMap);
        SecurityUtils.setUserDomainEntityPermissionMap(map);
    }

    private void setContactPersonPermissions(Set<CustomUserPermissionDto> permissionDtoSet, Long userId) {
        Set<String> projectSet = new HashSet<>();
        Set<String> subjectSet = new HashSet<>();
        Set<String> opportunitySet = new HashSet<>();
        Set<String> contactPersonSet = new HashSet<>();
        for (CustomUserPermissionDto permissionDto : permissionDtoSet) {
            if (permissionDto.getObjectName().equals(DomainEnum.PROJECT_DOMAIN_NAME.getValue())) {
                projectSet.add(permissionDto.getObjectId());
            }
            if (permissionDto.getObjectName().equals(DomainEnum.SUBJECT_DOMAIN_NAME.getValue())) {
                subjectSet.add(permissionDto.getObjectId());
            }
            if (permissionDto.getObjectName().equals(DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue())) {
                opportunitySet.add(permissionDto.getObjectId());
            }
        }

        Set<String> contactInProjectSet = contactPersonByObjectRepository
                .findIdByObjectTypeAndObjectIdSet(ContactPersonObjectTypeEnum.PROJECT.name(), projectSet);
        Set<String> contactInSubjectSet = contactPersonByObjectRepository
                .findIdByObjectTypeAndObjectIdSet(ContactPersonObjectTypeEnum.SUBJECT.name(), subjectSet);
        Set<String> contactInOpportunitySet = contactPersonByObjectRepository
                .findIdByObjectTypeAndObjectIdSet(ContactPersonObjectTypeEnum.OPORTUNITY.name(), opportunitySet);

        contactPersonSet.addAll(contactInProjectSet);
        contactPersonSet.addAll(contactInSubjectSet);
        contactPersonSet.addAll(contactInOpportunitySet);

        for (String contactPerson : contactPersonSet) {
            permissionDtoSet.add(new CustomUserPermissionDto(DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue(),
                    Permission.CONTACT_PERSON_VIEW.name(), userId, contactPerson, true));
        }

    }

    @Transactional
    public void deleteByPermissionId(String id) {
        Set<CustomUserPermissionEntity> toDelete = customUserPermissionRepository.getByPermissionId(id);
        Set<Long> userIdSet = new HashSet<>();
        for (CustomUserPermissionEntity entity : toDelete) {
            userIdSet.add(entity.getUserId());
        }
        appLogService.logDeleteMultiple(toDelete, CUSTOM_PERMISSION_OBJECT_NAME);
        customUserPermissionRepository.deleteAll(toDelete);
        for (Long userId : userIdSet) {
            loadPermissionSetByUser(userId);
        }
    }

    private void addPermissionToSet(Long userId, String userIdStr, String domain,
                                    Map<String, Map<String, Map<String, Set<String>>>> userDomainEntityPermMap,
                                    Set<CustomUserPermissionEntity> allEntitySet) {
        customUserPermissionRepository.deleteByUserIdAndObjectName(Long.valueOf(userIdStr), domain);
        appLogService.log("delete custom permissions",
                "object ".concat(domain)
                        .concat(" from user id ")
                        .concat(userIdStr), "");
        if (userDomainEntityPermMap.containsKey(userIdStr)
                && userDomainEntityPermMap.get(userIdStr).containsKey(domain)) {
            for (String entityId : userDomainEntityPermMap.get(userIdStr).get(domain).keySet()) {
                for (String perm : userDomainEntityPermMap.get(userIdStr).get(domain).get(entityId)) {
                    CustomUserPermissionEntity permissionEntity = new CustomUserPermissionEntity();
                    permissionEntity.setUserId(userId);
                    permissionEntity.setObjectName(domain);
                    permissionEntity.setObjectId(entityId);
                    permissionEntity.setPermissionId(perm);
                    permissionEntity.setCanView(Permission.valueOfOrNotExists(perm).canView());
                    allEntitySet.add(permissionEntity);
                }
            }
        }
    }
}
