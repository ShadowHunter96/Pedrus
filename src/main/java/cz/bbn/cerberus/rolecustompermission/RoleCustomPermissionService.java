package cz.bbn.cerberus.rolecustompermission;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactperson.dto.ContactPersonObjectTypeEnum;
import cz.bbn.cerberus.contactperson.persistance.repository.ContactPersonByObjectRepository;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.role.RoleService;
import cz.bbn.cerberus.rolecustompermission.dto.CustomRolePermissionDto;
import cz.bbn.cerberus.rolecustompermission.factory.CustomRolePermissionFactory;
import cz.bbn.cerberus.rolecustompermission.persistance.CustomRolePermissionEntity;
import cz.bbn.cerberus.rolecustompermission.persistance.CustomRolePermissionRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static cz.bbn.cerberus.custompermission.CustomPermissionService.CUSTOM_PERMISSION_OBJECT_NAME;

@Service
public class RoleCustomPermissionService {

    private final CustomRolePermissionRepository customRolePermissionRepository;
    private final ContactPersonByObjectRepository contactPersonByObjectRepository;
    private final AppLogService appLogService;
    private final RoleService roleService;

    public RoleCustomPermissionService(CustomRolePermissionRepository customRolePermissionRepository,
                                       AppLogService appLogService, RoleService roleService,
                                       ContactPersonByObjectRepository contactPersonByObjectRepository) {
        this.customRolePermissionRepository = customRolePermissionRepository;
        this.appLogService = appLogService;
        this.contactPersonByObjectRepository = contactPersonByObjectRepository;
        this.roleService = roleService;
        loadAllPermissionSet();
    }

    @Transactional
    public void save(Map<String, Map<String, Map<String, Set<String>>>> roleDomainEntityPermMap) {
        Set<CustomRolePermissionEntity> allEntitySet = new HashSet<>();
        for (Map.Entry<String, Map<String, Map<String, Set<String>>>> roleEntry : roleDomainEntityPermMap.entrySet()) {
            for (String domain : roleEntry.getValue().keySet()) {
                addPermissionToSet(roleEntry.getKey(), domain, roleDomainEntityPermMap, allEntitySet);
            }
        }
        customRolePermissionRepository.saveAll(allEntitySet);
        appLogService.logInsertMultiple(allEntitySet, CUSTOM_PERMISSION_OBJECT_NAME);
        for (String roleId : roleDomainEntityPermMap.keySet()) {
            loadPermissionSetByRole(roleId);
        }
    }

    public void loadAllPermissionSet() {
        for (String roleId : roleService.findAllId()) {
            loadPermissionSetByRole(roleId);
        }
    }

    public void loadPermissionSetByRole(String roleId) {
        Set<CustomRolePermissionDto> permissionDtoSet = ConvertEntities.fromEntities(
                customRolePermissionRepository.getByRoleId(roleId),
                CustomRolePermissionFactory::fromEntity);

        Map<String, Map<String, Set<String>>> domainEntityPermissionMap = new HashMap<>();

        for (CustomRolePermissionDto permissionDto : permissionDtoSet) {
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

        Map<String, Map<String, Map<String, Set<String>>>> map = SecurityUtils.getRoleDomainEntityPermissionMap();
        map.put(roleId, domainEntityPermissionMap);
        SecurityUtils.setRoleDomainEntityPermissionMap(map);
    }

    private void setContactPersonPermissions(Set<CustomRolePermissionDto> permissionDtoSet, String roleId) {
        Set<String> projectSet = new HashSet<>();
        Set<String> subjectSet = new HashSet<>();
        Set<String> opportunitySet = new HashSet<>();
        Set<String> contactPersonSet = new HashSet<>();
        for (CustomRolePermissionDto permissionDto : permissionDtoSet) {
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
            permissionDtoSet.add(new CustomRolePermissionDto(DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue(),
                    Permission.CONTACT_PERSON_VIEW.name(), roleId, contactPerson, true));
        }

    }

    private void addPermissionToSet(String roleId, String domain,
                                    Map<String, Map<String, Map<String, Set<String>>>> roleDomainEntityPermMap,
                                    Set<CustomRolePermissionEntity> allEntitySet) {
        customRolePermissionRepository.deleteByRoleIdAndObjectName(roleId, domain);
        appLogService.log("delete custom permissions",
                "object ".concat(domain)
                        .concat(" from role id ")
                        .concat(roleId), "");
        if (roleDomainEntityPermMap.containsKey(roleId)
                && roleDomainEntityPermMap.get(roleId).containsKey(domain)) {
            for (String entityId : roleDomainEntityPermMap.get(roleId).get(domain).keySet()) {
                for (String perm : roleDomainEntityPermMap.get(roleId).get(domain).get(entityId)) {
                    CustomRolePermissionEntity permissionEntity = new CustomRolePermissionEntity();
                    permissionEntity.setRoleId(roleId);
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
