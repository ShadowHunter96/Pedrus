package cz.bbn.cerberus.management;

import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.ContractService;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.offer.OfferService;
import cz.bbn.cerberus.offer.dto.OfferDto;
import cz.bbn.cerberus.opportunity.OpportunityService;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.permissionmanagement.dto.CheckItem;
import cz.bbn.cerberus.permissionmanagement.dto.OwnerEntityDto;
import cz.bbn.cerberus.project.ProjectService;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.subject.SubjectService;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.user.dto.UserDto;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

@Service
public class OwnerService {

    private final ListService listService;
    private final SubjectService subjectService;
    private final OpportunityService opportunityService;
    private final ProjectService projectService;
    private final OfferService offerService;
    private final ContractService contractService;
    private final CustomPermissionService customPermissionService;

    public OwnerService(ListService listService, SubjectService subjectService, OpportunityService opportunityService,
                        ProjectService projectService, OfferService offerService, ContractService contractService,
                        CustomPermissionService customPermissionService) {
        this.listService = listService;
        this.subjectService = subjectService;
        this.opportunityService = opportunityService;
        this.projectService = projectService;
        this.offerService = offerService;
        this.contractService = contractService;
        this.customPermissionService = customPermissionService;
    }

    public List<OwnerEntityDto> getEntityOwnerList(Set<UserDto> userDtoSet, ObjectType objectType) {
        switch (objectType) {
            case SUBJECT -> {
                List<SubjectDto> subjectDtoList = listService.getSubjectDtoList().stream().filter(subjectDto ->
                        !Boolean.TRUE.equals(subjectDto.getDeleted())
                                && userDtoSet.contains(subjectDto.getUserDto())).collect(Collectors.toList());
                return subjectDtoListToMap(subjectDtoList, userDtoSet);
            }
            case OPPORTUNITY -> {
                List<OpportunityDto> opportunityDtoList = listService.getOpportunityDtoList().stream().filter(opportunityDto ->
                        !Boolean.TRUE.equals(opportunityDto.getDeleted())
                                && userDtoSet.contains(opportunityDto.getUser())).collect(Collectors.toList());
                return opportunityDtoListToMap(opportunityDtoList, userDtoSet);
            }
            case CONTRACT -> {
                List<ContractDto> contractDtoList = listService.getContractList().stream().filter(opportunityDto ->
                        !Boolean.TRUE.equals(opportunityDto.getDeleted())
                                && userDtoSet.contains(opportunityDto.getUserDto())).collect(Collectors.toList());
                return contractDtoListToMap(contractDtoList, userDtoSet);
            }
            case PROJECT -> {
                List<ProjectDto> projectDtoList = listService.getProjectDtoList().stream().filter(opportunityDto ->
                        !Boolean.TRUE.equals(opportunityDto.isDeleted())
                                && userDtoSet.contains(opportunityDto.getUserDto())).collect(Collectors.toList());
                return projectDtoListToMap(projectDtoList, userDtoSet);
            }
            case OFFER -> {
                List<OfferDto> offerDtoList = listService.getOfferDtoList().stream().filter(opportunityDto ->
                        !Boolean.TRUE.equals(opportunityDto.getDeleted())
                                && userDtoSet.contains(opportunityDto.getProcessedByUserDto())).collect(Collectors.toList());
                return offerDtoListToMap(offerDtoList, userDtoSet);
            }

        }
        return new ArrayList<>();
    }

    private List<OwnerEntityDto> subjectDtoListToMap(List<SubjectDto> subjectDtoList, Set<UserDto> userDtoSet) {
        List<OwnerEntityDto> list = new ArrayList<>();
        subjectDtoList.forEach(subjectDto -> {
            OwnerEntityDto ownerEntityDto = new OwnerEntityDto();
            ownerEntityDto.setEntityId(subjectDto.getId());
            ownerEntityDto.setEntityName(subjectDto.getName());
            ownerEntityDto.setObjectType(ObjectType.SUBJECT);
            List<CheckItem> ownerCheckList = new ArrayList<>();
            List<CheckItem> editPermissionCheckList = new ArrayList<>();
            userDtoSet.forEach(userDto -> {
                ownerCheckList.add(new CheckItem(userDto.getId(),
                        userDto.getId().equals(subjectDto.getUserDto().getId())));
                boolean editPermission = SecurityUtils.hasCustomUserPermission(DomainEnum.SUBJECT_DOMAIN_NAME.getValue(),
                        subjectDto.getId(), Permission.SUBJECT_EDIT.name(), userDto.getId());
                editPermissionCheckList.add(new CheckItem(userDto.getId(), editPermission));
                ownerEntityDto.setEditPermissionList(editPermissionCheckList);
            });
            ownerEntityDto.setCheckedItemList(ownerCheckList);
            ownerEntityDto.setEditPermissionList(editPermissionCheckList);
            list.add(ownerEntityDto);
        });
        return list;
    }

    private List<OwnerEntityDto> contractDtoListToMap(List<ContractDto> contractDtoList, Set<UserDto> userDtoSet) {
        List<OwnerEntityDto> list = new ArrayList<>();
        contractDtoList.forEach(contractDto -> {
            OwnerEntityDto ownerEntityDto = new OwnerEntityDto();
            ownerEntityDto.setEntityId(contractDto.getId());
            ownerEntityDto.setEntityName(contractDto.getName());
            ownerEntityDto.setObjectType(ObjectType.CONTRACT);
            List<CheckItem> checkItemList = new ArrayList<>();
            List<CheckItem> editPermissionCheckList = new ArrayList<>();
            userDtoSet.forEach(userDto -> {
                checkItemList.add(new CheckItem(userDto.getId(),
                        userDto.getId().equals(contractDto.getUserDto().getId())));
                boolean editPermission = SecurityUtils.hasCustomPermission(DomainEnum.CONTRACT_DOMAIN_NAME.getValue(),
                        contractDto.getId(), Permission.CONTRACT_EDIT.name());
                editPermissionCheckList.add(new CheckItem(userDto.getId(), editPermission));
            });
            ownerEntityDto.setCheckedItemList(checkItemList);
            ownerEntityDto.setEditPermissionList(editPermissionCheckList);
            list.add(ownerEntityDto);
        });
        return list;
    }

    private List<OwnerEntityDto> projectDtoListToMap(List<ProjectDto> contractDtoList, Set<UserDto> userDtoSet) {
        List<OwnerEntityDto> list = new ArrayList<>();
        contractDtoList.forEach(projectDto -> {
            OwnerEntityDto ownerEntityDto = new OwnerEntityDto();
            ownerEntityDto.setEntityId(projectDto.getId());
            ownerEntityDto.setEntityName(projectDto.getName());
            ownerEntityDto.setObjectType(ObjectType.PROJECT);
            List<CheckItem> checkItemList = new ArrayList<>();
            List<CheckItem> editPermissionCheckList = new ArrayList<>();
            userDtoSet.forEach(userDto -> {
                checkItemList.add(new CheckItem(userDto.getId(),
                        userDto.getId().equals(projectDto.getUserDto().getId())));
                boolean editPermission = SecurityUtils.hasCustomPermission(DomainEnum.PROJECT_DOMAIN_NAME.getValue(),
                        projectDto.getId(), Permission.PROJECT_EDIT.name());
                editPermissionCheckList.add(new CheckItem(userDto.getId(), editPermission));
            });
            ownerEntityDto.setCheckedItemList(checkItemList);
            ownerEntityDto.setEditPermissionList(editPermissionCheckList);
            list.add(ownerEntityDto);
        });
        return list;
    }

    private List<OwnerEntityDto> offerDtoListToMap(List<OfferDto> contractDtoList, Set<UserDto> userDtoSet) {
        List<OwnerEntityDto> list = new ArrayList<>();
        contractDtoList.forEach(offerDto -> {
            OwnerEntityDto ownerEntityDto = new OwnerEntityDto();
            ownerEntityDto.setEntityId(offerDto.getId());
            ownerEntityDto.setEntityName(offerDto.getName());
            ownerEntityDto.setObjectType(ObjectType.OFFER);
            List<CheckItem> checkItemList = new ArrayList<>();
            List<CheckItem> editPermissionCheckList = new ArrayList<>();
            userDtoSet.forEach(userDto -> {
                checkItemList.add(new CheckItem(userDto.getId(),
                        userDto.getId().equals(offerDto.getProcessedByUserDto().getId())));
                boolean editPermission = SecurityUtils.hasCustomPermission(DomainEnum.OFFER_DOMAIN_NAME.getValue(),
                        offerDto.getId(), Permission.OFFER_EDIT.name());
                editPermissionCheckList.add(new CheckItem(userDto.getId(), editPermission));
            });
            ownerEntityDto.setCheckedItemList(checkItemList);
            ownerEntityDto.setEditPermissionList(editPermissionCheckList);
            list.add(ownerEntityDto);
        });
        return list;
    }

    private List<OwnerEntityDto> opportunityDtoListToMap(List<OpportunityDto> opportunityDtoList, Set<UserDto> userDtoSet) {
        List<OwnerEntityDto> list = new ArrayList<>();
        opportunityDtoList.forEach(opportunityDto -> {
            OwnerEntityDto ownerEntityDto = new OwnerEntityDto();
            ownerEntityDto.setEntityId(opportunityDto.getId());
            ownerEntityDto.setEntityName(opportunityDto.getName());
            ownerEntityDto.setObjectType(ObjectType.OPPORTUNITY);
            List<CheckItem> checkItemList = new ArrayList<>();
            List<CheckItem> editPermissionCheckList = new ArrayList<>();
            userDtoSet.forEach(userDto -> {
                    checkItemList.add(new CheckItem(userDto.getId(),
                            userDto.getId().equals(opportunityDto.getUser().getId())));
                boolean editPermission = SecurityUtils.hasCustomPermission(DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue(),
                        opportunityDto.getId(), Permission.OPPORTUNITY_EDIT.name());
                editPermissionCheckList.add(new CheckItem(userDto.getId(), editPermission));
                ownerEntityDto.setEditPermissionList(editPermissionCheckList);

            });
            ownerEntityDto.setCheckedItemList(checkItemList);
            ownerEntityDto.setEditPermissionList(editPermissionCheckList);
            list.add(ownerEntityDto);
        });
        return list;
    }

    public void saveOwnerEntityList(List<OwnerEntityDto> ownerEntityDtoList, ObjectType objectType, Set<UserDto> userDtoSet){
                Map<Long, Map<String, Map<String, Set<String>>>> userPermissionMap = SecurityUtils.getUserDomainEntityPermissionMap();
                Map<String, Map<String, Map<String, Set<String>>>> actualMap = new HashMap<>();
                userPermissionMap.forEach((aLong, stringMapMap) -> {
                    userDtoSet.forEach(userDto -> {
                        if(userDto.getId() == aLong){
                            actualMap.put(String.valueOf(aLong), stringMapMap);
                        }
                    });
                });

                ownerEntityDtoList.forEach(ownerEntityDto -> {
                    AtomicReference<Long> userId = new AtomicReference<>(0L);
                    ownerEntityDto.getCheckedItemList().forEach(checkItem -> {
                        if(checkItem.isChecked()){
                            userId.set(checkItem.getUserId());
                        }
                    });
                    if(userId.get() != null) {
                        save(objectType, userId.get(), ownerEntityDto.getEntityId(), actualMap);
                    }
                });
                customPermissionService.save(actualMap);
                reload(objectType);
    }

    public void bulkSave(Set<ObjectType> objectTypeSet, UserDto owner, UserDto newOwner){
        Map<Long, Map<String, Map<String, Set<String>>>> userPermissionMap = SecurityUtils.getUserDomainEntityPermissionMap();
        Map<String, Map<String, Map<String, Set<String>>>> actualMap = new HashMap<>();
        userPermissionMap.forEach((aLong, stringMapMap) -> {
                if(newOwner.getId() == aLong) {
                    actualMap.put(String.valueOf(aLong), stringMapMap);
            }
        });
        objectTypeSet.forEach(objectType -> {
            switch (objectType){
                case OPPORTUNITY -> {
                    opportunityService.updateOwner(owner.getId(), newOwner.getId());
                    List<OwnerEntityDto> ownerEntityDtoList = getEntityOwnerList(Collections.singleton(owner), objectType);
                    ownerEntityDtoList.forEach(ownerEntityDto ->
                            addAllPermissions(actualMap.get(String.valueOf(newOwner.getId())), DomainEnum.OPPORTUNITY_DOMAIN_NAME, ownerEntityDto.getEntityId()));
                }
                case SUBJECT -> {
                    subjectService.updateOwner(owner.getId(), newOwner.getId());
                    List<OwnerEntityDto> ownerEntityDtoList = getEntityOwnerList(Collections.singleton(owner), objectType);
                    ownerEntityDtoList.forEach(ownerEntityDto ->
                            addAllPermissions(actualMap.get(String.valueOf(newOwner.getId())), DomainEnum.SUBJECT_DOMAIN_NAME, ownerEntityDto.getEntityId()));
                }
                case OFFER -> {
                    offerService.updateOwner(owner.getId(), newOwner.getId());
                    List<OwnerEntityDto> ownerEntityDtoList = getEntityOwnerList(Collections.singleton(owner), objectType);
                    ownerEntityDtoList.forEach(ownerEntityDto ->
                            addAllPermissions(actualMap.get(String.valueOf(newOwner.getId())), DomainEnum.OFFER_DOMAIN_NAME, ownerEntityDto.getEntityId()));
                }
                case CONTRACT -> {
                    contractService.updateOwner(owner.getId(), newOwner.getId());
                    List<OwnerEntityDto> ownerEntityDtoList = getEntityOwnerList(Collections.singleton(owner), objectType);
                    ownerEntityDtoList.forEach(ownerEntityDto ->
                            addAllPermissions(actualMap.get(String.valueOf(newOwner.getId())), DomainEnum.CONTRACT_DOMAIN_NAME, ownerEntityDto.getEntityId()));
                }
                case PROJECT -> {
                    projectService.updateOwner(owner.getId(), newOwner.getId());
                    List<OwnerEntityDto> ownerEntityDtoList = getEntityOwnerList(Collections.singleton(owner), objectType);
                    ownerEntityDtoList.forEach(ownerEntityDto ->
                            addAllPermissions(actualMap.get(String.valueOf(newOwner.getId())), DomainEnum.PROJECT_DOMAIN_NAME, ownerEntityDto.getEntityId()));
                }
            }
            customPermissionService.save(actualMap);
            reload(objectType);
        });
    }


    private void reload(ObjectType objectType){
        switch (objectType){
            case SUBJECT -> listService.reloadSubjectDtoList();
            case PROJECT -> listService.reloadProjectList();
            case OFFER -> listService.reloadOfferDtoList();
            case CONTRACT -> listService.reloadContractList();
            case OPPORTUNITY -> listService.reloadOpportunityDtoList();
        }
    }

    private void save(ObjectType objectType, Long userId, String entityId, Map<String, Map<String, Map<String, Set<String>>>> actualMap){
        switch (objectType) {
            case SUBJECT -> {
                subjectService.updateOwner(userId, entityId);
                addAllPermissions(actualMap.get(String.valueOf(userId)), DomainEnum.SUBJECT_DOMAIN_NAME, entityId);
            }
            case PROJECT -> {
                projectService.updateOwner(userId, entityId);
                addAllPermissions(actualMap.get(String.valueOf(userId)), DomainEnum.PROJECT_DOMAIN_NAME, entityId);
            }
            case OFFER -> {
                offerService.updateOwner(userId, entityId);
                addAllPermissions(actualMap.get(String.valueOf(userId)), DomainEnum.OFFER_DOMAIN_NAME, entityId);
            }
            case CONTRACT -> {
                contractService.updateOwner(userId, entityId);
                addAllPermissions(actualMap.get(String.valueOf(userId)), DomainEnum.CONTRACT_DOMAIN_NAME, entityId);
            }
            case OPPORTUNITY -> {
                opportunityService.updateOwner(userId, entityId);
                addAllPermissions(actualMap.get(String.valueOf(userId)), DomainEnum.OPPORTUNITY_DOMAIN_NAME, entityId);
            }
        }
    }

    private void addAllPermissions(Map<String, Map<String, Set<String>>> actualMap, DomainEnum domainEnum, String item){
        if(!actualMap.containsKey(domainEnum.getValue())){
            actualMap.put(domainEnum.getValue(), new HashMap<>());
        }
        actualMap.get(domainEnum.getValue()).put(item, Collections.singleton(CustomPermissionService.ALL_PERMISSION));
    }
}
