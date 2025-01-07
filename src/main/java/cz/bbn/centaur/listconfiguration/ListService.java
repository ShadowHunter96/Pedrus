package cz.bbn.cerberus.listconfiguration;

import cz.bbn.cerberus.area.AreaService;
import cz.bbn.cerberus.area.dto.AreaDto;
import cz.bbn.cerberus.asset.dto.AssetDto;
import cz.bbn.cerberus.asset.factory.AssetFactory;
import cz.bbn.cerberus.asset.persistance.repository.AssetRepository;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.security.AppUser;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactperson.dto.ContactPersonDto;
import cz.bbn.cerberus.contactperson.factory.ContactPersonFactory;
import cz.bbn.cerberus.contactperson.persistance.repository.ContactPersonRepository;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.contract.factory.ContractFactory;
import cz.bbn.cerberus.contract.persistence.repository.ContractRepository;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.contracttype.factory.ContractTypeFactory;
import cz.bbn.cerberus.contracttype.persistence.ContractTypeRepository;
import cz.bbn.cerberus.dssetting.dto.DsSettingSimpleDto;
import cz.bbn.cerberus.dssetting.factory.DsSettingFactory;
import cz.bbn.cerberus.dssetting.persistance.DsSettingSimpleRepository;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.employee.factory.EmployeeFactory;
import cz.bbn.cerberus.employee.persistance.repository.EmployeeRepository;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.enumeration.factory.EnumerationFactory;
import cz.bbn.cerberus.enumeration.persistance.repository.EnumerationRepository;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.invoice.factory.InvoiceFactory;
import cz.bbn.cerberus.invoice.persistance.repository.InvoiceRepository;
import cz.bbn.cerberus.offer.dto.OfferDto;
import cz.bbn.cerberus.offer.factory.OfferFactory;
import cz.bbn.cerberus.offer.repository.repository.OfferRepository;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.opportunity.factory.OpportunityFactory;
import cz.bbn.cerberus.opportunity.persistance.repository.OpportunityRepository;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.project.factory.ProjectFactory;
import cz.bbn.cerberus.project.persistance.repository.ProjectRepository;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.subject.factory.SubjectFactory;
import cz.bbn.cerberus.subject.persistance.SubjectRepository;
import cz.bbn.cerberus.suppliertype.dto.SupplierTypeDto;
import cz.bbn.cerberus.suppliertype.factory.SupplierTypeFactory;
import cz.bbn.cerberus.suppliertype.persistance.SupplierTypeRepository;
import cz.bbn.cerberus.technology.TechnologyService;
import cz.bbn.cerberus.technology.dto.TechnologyDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserRepository;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Service
public class ListService {

    private final InvoiceRepository invoiceRepository;
    private final ContactPersonRepository contactPersonRepository;
    private final ContractRepository contractRepository;
    private final ProjectRepository projectRepository;
    private final SubjectRepository subjectRepository;
    private final SupplierTypeRepository supplierTypeRepository;
    private final OpportunityRepository opportunityRepository;
    private final DsSettingSimpleRepository dsSettingSimpleRepository;
    private final ContractTypeRepository contractTypeRepository;
    private final AssetRepository assetRepository;
    private final EnumerationRepository enumerationRepository;
    private final AppEnv appEnv;
    private final OfferRepository offerRepository;
    private final EmployeeRepository employeeRepository;
    private final TechnologyService technologyService;
    private final AreaService areaService;
    private final UserRepository userRepository;

    private List<InvoiceDto> invoiceDtoList;
    private List<ContactPersonDto> contactPersonDtoList;
    private List<ContractDto> contractDtoList;
    private List<ProjectDto> projectDtoList;
    private List<SubjectDto> subjectDtoList;
    private List<SupplierTypeDto> supplierTypeDtoList;
    private List<OpportunityDto> opportunityDtoList;
    private List<OfferDto> offerDtoList;
    private List<DsSettingSimpleDto> dsSettingSimpleDtoList;
    private List<ContractTypeDto> contractTypeDtoList;
    private List<AssetDto> assetDtoList;
    private List<EnumerationDto> enumerationDtoList;
    private List<EmployeeDto> employeeDtoList;
    private List<TechnologyDto> technologyDtoList;
    private List<AreaDto> areaDtoList;
    private List<UserDto> userDtoList;

    public ListService(InvoiceRepository invoiceRepository, ContactPersonRepository contactPersonRepository,
                       ContractRepository contractRepository, ProjectRepository projectRepository,
                       SubjectRepository subjectRepository, SupplierTypeRepository supplierTypeRepository,
                       OpportunityRepository opportunityRepository,
                       DsSettingSimpleRepository dsSettingSimpleRepository,
                       ContractTypeRepository contractTypeRepository,
                       AssetRepository assetRepository, EnumerationRepository enumerationRepository,
                       AppEnv appEnv, OfferRepository offerRepository, EmployeeRepository employeeRepository,
                       TechnologyService technologyService, AreaService areaService, UserRepository userRepository) {
        this.invoiceRepository = invoiceRepository;
        this.contactPersonRepository = contactPersonRepository;
        this.contractRepository = contractRepository;
        this.projectRepository = projectRepository;
        this.subjectRepository = subjectRepository;
        this.supplierTypeRepository = supplierTypeRepository;
        this.opportunityRepository = opportunityRepository;
        this.dsSettingSimpleRepository = dsSettingSimpleRepository;
        this.contractTypeRepository = contractTypeRepository;
        this.assetRepository = assetRepository;
        this.enumerationRepository = enumerationRepository;
        this.appEnv = appEnv;
        this.offerRepository = offerRepository;
        this.employeeRepository = employeeRepository;
        this.technologyService = technologyService;
        this.areaService = areaService;
        this.userRepository = userRepository;
    }

    public void reloadAll() {
        reloadInvoiceList();
        reloadContactPersonList();
        reloadContractList();
        reloadProjectList();
        reloadSubjectDtoList();
        reloadOpportunityDtoList();
        reloadSupplierTypeDtoList();
        reloadDsSettingsList();
        reloadContractTypeDtoList();
        reloadAssetDtoList();
        reloadEnumerationList();
        reloadOfferDtoList();
        reloadEmployeeDtoList();
        reloadAreaDtoList();
        reloadTechnologyDtoList();
        reloadUserDtoList();
        SuccessNotification.show(Transl.get("Cash reloaded"), appEnv);
    }

    public String getNameByIdAndObjectType(String id, String objectType) {
        if (DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue().equals(objectType)) {
            for (ContactPersonDto contactPersonDto : getContactPersonList()) {
                if (contactPersonDto.getId().equals(id)) {
                    return contactPersonDto.getName();
                }
            }
        }
        if (DomainEnum.CONTRACT_DOMAIN_NAME.getValue().equals(objectType)) {
            for (ContractDto contractDto : getContractList()) {
                if (contractDto.getId().equals(id)) {
                    return contractDto.getName();
                }
            }
        }
        if (DomainEnum.PROJECT_DOMAIN_NAME.getValue().equals(objectType)) {
            for (ProjectDto projectDto : getProjectDtoList()) {
                if (projectDto.getId().equals(id)) {
                    return projectDto.getName();
                }
            }
        }
        if (DomainEnum.SUBJECT_DOMAIN_NAME.getValue().equals(objectType)) {
            for (SubjectDto subjectDto : getSubjectDtoList()) {
                if (subjectDto.getId().equals(id)) {
                    return subjectDto.getName();
                }
            }
        }

        if (DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue().equals(objectType)) {
            for (OpportunityDto opportunityDto : getOpportunityDtoList()) {
                if (opportunityDto.getId().equals(id)) {
                    return opportunityDto.getName();
                }
            }
        }

        if (DomainEnum.DS_SETTING_DOMAIN_NAME.getValue().equals(objectType)) {
            for (DsSettingSimpleDto dsSettingSimpleDto : getDsSettingsDtoDtoList()) {
                if (dsSettingSimpleDto.getId().equals(id)) {
                    return dsSettingSimpleDto.getName();
                }
            }
        }
        return id;
    }

    public List<EnumerationDto> getEnumerationDtoList(String enumerationTypeId) {
        return getEnumerationDtoList(enumerationTypeId, false);
    }

    public List<EnumerationDto> getEnumerationDtoList(String enumerationTypeId, boolean witNotAllowed) {
        if (enumerationDtoList == null) {
            reloadEnumerationList();
        }
        return new ArrayList<>(
                enumerationDtoList.stream()
                        .filter(enumerationDto ->
                                enumerationDto.getEnumerationTypeDto().getId().equalsIgnoreCase(enumerationTypeId) &&
                                        (witNotAllowed ? Boolean.TRUE : enumerationDto.getAllowed()) &&
                                        Boolean.FALSE.equals(enumerationDto.getDeleted()))
                        .toList()
        );
    }

    public List<InvoiceDto> getInvoiceDtoList() {
        if (invoiceDtoList == null) {
            reloadInvoiceList();
        }
        return new ArrayList<>(invoiceDtoList);
    }

    public void reloadInvoiceList() {
        invoiceDtoList = ConvertEntities.fromEntities(invoiceRepository.findAll(), InvoiceFactory::fromEntity);
    }

    public void reloadTechnologyDtoList() {
        technologyDtoList = technologyService.findAllowedTechnologyDtoList();
    }

    public List<UserDto> getUserDtoList() {
        if (userDtoList == null) {
            reloadUserDtoList();
        }
        return new ArrayList<>(userDtoList);
    }

    public void reloadUserDtoList() {
        userDtoList = ConvertEntities.fromEntities(userRepository.findAllAllowed(), UserFactory::fromEntity);
    }

    public void reloadAreaDtoList() {
        areaDtoList = areaService.findAllNotDeletedAreaDtoList();
    }

    public List<ContactPersonDto> getContactPersonList() {
        if (contactPersonDtoList == null) {
            reloadContactPersonList();
        }
        return new ArrayList<>(contactPersonDtoList);
    }

    public void reloadContactPersonList() {
        contactPersonDtoList = ConvertEntities.fromEntities(
                contactPersonRepository.findAll(), ContactPersonFactory::fromEntity);
    }

    public void reloadEnumerationList() {
        enumerationDtoList = ConvertEntities.fromEntities(
                enumerationRepository.findAll(), EnumerationFactory::fromEntity);
    }

    public void reloadOfferDtoList() {
        offerDtoList = ConvertEntities.fromEntities(offerRepository.findAllNotDeleted(), OfferFactory::fromEntity);
    }

    public void reloadEmployeeDtoList() {
        employeeDtoList = ConvertEntities.fromEntities(employeeRepository.findValidList(), EmployeeFactory::fromEntity);
    }

    public List<OfferDto> getOfferDtoList() {
        if (offerDtoList == null) {
            reloadOfferDtoList();
        }
        return new ArrayList<>(offerDtoList);
    }

    public List<ContractDto> getContractList() {
        if (contractDtoList == null) {
            reloadContractList();
        }
        return new ArrayList<>(contractDtoList);
    }

    public List<TechnologyDto> getTechnologyList() {
        if (technologyDtoList == null) {
            reloadTechnologyDtoList();
        }
        return new ArrayList<>(technologyDtoList);
    }

    public List<AreaDto> getAreaList() {
        if (areaDtoList == null) {
            reloadAreaDtoList();
        }
        return new ArrayList<>(areaDtoList);
    }

    public void reloadContractList() {
        contractDtoList = ConvertEntities.fromEntities(contractRepository.findAll(), ContractFactory::fromEntity);
    }

    public List<EmployeeDto> getEmployeeDtoList() {
        if (employeeDtoList == null) {
            reloadEmployeeDtoList();
        }
        return new ArrayList<>(employeeDtoList);
    }

    public List<EmployeeDto> getEmployeeDtoListForApprover() {
        Long userId = SecurityUtils.getCurrentUserId();
        return getEmployeeDtoList().stream().filter(employeeDto ->
                (employeeDto.getLineManagerUserDto() != null && employeeDto.getLineManagerUserDto().getId().equals(userId))
                        || (employeeDto.getSuperiorUserDto() != null && employeeDto.getSuperiorUserDto().getId().equals(userId))
        ).toList();
    }

    public Map<String, ContractDto> getContractMap() {
        Map<String, ContractDto> contractMap = new HashMap<>();
        for (ContractDto contract : getContractList()) {
            contractMap.put(contract.getId(), contract);
        }
        return contractMap;
    }

    public List<ProjectDto> getProjectDtoList() {
        if (projectDtoList == null) {
            reloadProjectList();
        }
        return new ArrayList<>(projectDtoList);
    }

    public void reloadProjectList() {
        projectDtoList = ConvertEntities.fromEntities(projectRepository.findAll(), ProjectFactory::fromEntity);
    }

    public Map<String, ProjectDto> getProjectDtoMap() {
        Map<String, ProjectDto> projectMap = new HashMap<>();
        for (ProjectDto projectDto : getProjectDtoList()) {
            projectMap.put(projectDto.getId(), projectDto);
        }
        return projectMap;
    }

    public List<ProjectDto> getAllowedProjectDtoList() {
        if (SecurityUtils.hasPermission(Permission.PROJECT_LIST_VIEW)
                && SecurityUtils.hasCustomReadAll(DomainEnum.PROJECT_DOMAIN_NAME.getValue())) {
            return getProjectDtoList().stream()
                    .filter(projectDto -> !projectDto.isDeleted())
                    .toList();
        } else {
            Set<String> permissionSet = SecurityUtils.getCustomReadPermission(DomainEnum.PROJECT_DOMAIN_NAME.getValue());
            return getProjectDtoList().stream()
                    .filter(projectDto -> !projectDto.isDeleted() && permissionSet.contains(projectDto.getId()))
                    .toList();
        }
    }

    public void reloadDsSettingsList() {
        dsSettingSimpleDtoList = ConvertEntities.fromEntities(
                dsSettingSimpleRepository.findAll(), DsSettingFactory::fromEntity);
    }

    public List<SubjectDto> getSubjectDtoListByCustomer() {
        Set<String> subjectIdSet = SecurityUtils.getCustomReadPermission(DomainEnum.SUBJECT_DOMAIN_NAME.getValue());
        return getSubjectDtoList()
                .stream()
                .filter(subjectDto -> Boolean.TRUE == subjectDto.getCustomer() &&
                        Boolean.FALSE == subjectDto.getDeleted() &&
                        subjectIdSet.contains(subjectDto.getId()))
                .toList();
    }

    public List<SubjectDto> getSubjectDtoListByOwnCompany() {
        return getSubjectDtoList()
                .stream()
                .filter(subjectDto ->
                        Boolean.TRUE.equals(subjectDto.getOwnCompany()) && Boolean.FALSE.equals(subjectDto.getDeleted()))
                .toList();
    }

    public List<SubjectDto> getSubjectDtoListEditPermission(AppUser appUser) {
        Set<String> permissionSet = SecurityUtils.getAllowedEntityIdByDomain(Permission.SUBJECT_EDIT.getObjectName(),
                DomainEnum.SUBJECT_DOMAIN_NAME.getValue(), appUser);
        return getSubjectDtoList().stream().filter(subjectDto ->
                        subjectDto.getDeleted().equals(Boolean.FALSE) && permissionSet.contains(subjectDto.getId()))
                .toList();
    }

    public List<ProjectDto> getProjectDtoListEditPermission(AppUser appUser) {
        Set<String> permissionSet = SecurityUtils.getAllowedEntityIdByDomain(Permission.PROJECT_EDIT.getObjectName(),
                DomainEnum.PROJECT_DOMAIN_NAME.getValue(), appUser);
        return getProjectDtoList().stream().filter(projectDto ->
                        !projectDto.isDeleted() && permissionSet.contains(projectDto.getId()))
                .toList();
    }

    public List<ContractDto> getContractDtoListEditPermission(AppUser appUser) {
        Set<String> permissionSet = SecurityUtils.getAllowedEntityIdByDomain(Permission.CONTRACT_EDIT.getObjectName(),
                DomainEnum.CONTRACT_DOMAIN_NAME.getValue(), appUser);
        return getContractList().stream().filter(contractDto ->
                        contractDto.getDeleted().equals(Boolean.FALSE) && permissionSet.contains(contractDto.getId()))
                .toList();
    }

    public List<OfferDto> getOfferDtoListEditPermission(AppUser appUser) {
        Set<String> permissionSet = SecurityUtils.getAllowedEntityIdByDomain(Permission.OFFER_EDIT.getObjectName(),
                DomainEnum.OFFER_DOMAIN_NAME.getValue(), appUser);
        return getOfferDtoList().stream().filter(offerDto ->
                        offerDto.getDeleted().equals(Boolean.FALSE) && permissionSet.contains(offerDto.getId()))
                .toList();
    }

    public List<OpportunityDto> getOpportunityDtoListEditPermission(AppUser appUser) {
        Set<String> permissionSet = SecurityUtils.getAllowedEntityIdByDomain(Permission.OPPORTUNITY_EDIT.getObjectName(),
                DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue(), appUser);
        return getOpportunityDtoList().stream().filter(offerDto ->
                        offerDto.getDeleted().equals(Boolean.FALSE) && permissionSet.contains(offerDto.getId()))
                .toList();
    }

    public List<SubjectDto> getSubjectDtoListByOwnCompanyOrSupplier() {
        return getSubjectDtoList()
                .stream()
                .filter(subjectDto ->
                        (Boolean.TRUE.equals(subjectDto.getOwnCompany()) || Boolean.TRUE.equals(subjectDto.getSupplier()))
                                && Boolean.FALSE.equals(subjectDto.getDeleted()))
                .toList();
    }

    public List<SubjectDto> getSubjectDtoList() {
        if (subjectDtoList == null) {
            reloadSubjectDtoList();
        }
        return new ArrayList<>(subjectDtoList);
    }

    public void reloadSubjectDtoList() {
        subjectDtoList = ConvertEntities.fromEntities(subjectRepository.findAll(), SubjectFactory::fromEntity);
    }

    public Map<String, SubjectDto> getSubjectMap() {
        Map<String, SubjectDto> subjectMap = new HashMap<>();
        for (SubjectDto subject : getSubjectDtoList()) {
            subjectMap.put(subject.getId(), subject);
        }
        return subjectMap;
    }

    public List<SupplierTypeDto> getSupplierTypeDtoList() {
        if (supplierTypeDtoList == null) {
            reloadSupplierTypeDtoList();
        }
        return new ArrayList<>(supplierTypeDtoList);
    }

    public void reloadSupplierTypeDtoList() {
        supplierTypeDtoList =
                ConvertEntities.fromEntities(supplierTypeRepository.findAll(), SupplierTypeFactory::fromEntity);
    }

    public Map<String, SupplierTypeDto> getSupplierTypeMap() {
        Map<String, SupplierTypeDto> supplierTypeMap = new HashMap<>();
        for (SupplierTypeDto supplierType : getSupplierTypeDtoList()) {
            supplierTypeMap.put(supplierType.getId(), supplierType);
        }
        return supplierTypeMap;
    }

    public void reloadOpportunityDtoList() {
        opportunityDtoList = ConvertEntities.fromEntities(
                opportunityRepository.findAll(), OpportunityFactory::fromEntity);
    }

    public List<OpportunityDto> getOpportunityDtoList() {
        if (opportunityDtoList == null) {
            reloadOpportunityDtoList();
        }
        return opportunityDtoList;
    }

    public List<OpportunityDto> getOpportunityListReadPermission() {
        if (SecurityUtils.hasCustomReadAll(DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue())) {
            return getOpportunityDtoList()
                    .stream()
                    .filter(opportunityDto ->
                            !Boolean.TRUE.equals(opportunityDto.getDeleted()))
                    .toList();
        } else {
            Set<String> set = SecurityUtils.getCustomReadPermission(
                    DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue());
            return getOpportunityDtoList()
                    .stream()
                    .filter(opportunityDto ->
                            !Boolean.TRUE.equals(opportunityDto.getDeleted()) && set.contains(opportunityDto.getId()))
                    .toList();
        }
    }


    public List<OpportunityDto> getOpportunityDtoListByCustomer(String customerId) {
        return getOpportunityDtoList()
                .stream()
                .filter(opportunityDto -> opportunityDto.getSubject().getId().equals(customerId))
                .toList();
    }

    public Map<String, OpportunityDto> getOpportunityMap() {
        Map<String, OpportunityDto> opportunityMap = new HashMap<>();
        for (OpportunityDto opportunityDto : getOpportunityDtoList()) {
            opportunityMap.put(opportunityDto.getId(), opportunityDto);
        }
        return opportunityMap;
    }

    public List<DsSettingSimpleDto> getDsSettingsDtoDtoList() {
        if (dsSettingSimpleDtoList == null) {
            reloadDsSettingsList();
        }
        return dsSettingSimpleDtoList;
    }

    public void reloadContractTypeDtoList() {
        contractTypeDtoList = ConvertEntities.fromEntities(
                contractTypeRepository.findAll(), ContractTypeFactory::fromEntity);
    }

    public List<ContractTypeDto> getContractTypeDtoList() {
        if (contractTypeDtoList == null) {
            reloadContractTypeDtoList();
        }
        return contractTypeDtoList;
    }

    public void reloadAssetDtoList() {
        assetDtoList = ConvertEntities.fromEntities(assetRepository.findAll(), AssetFactory::fromEntity);
    }

    public List<AssetDto> getAssetDtoList() {
        if (assetDtoList == null) {
            reloadAssetDtoList();
        }
        return assetDtoList;
    }

    public Map<String, AssetDto> getAssetMap() {
        Map<String, AssetDto> assetMap = new HashMap<>();
        for (AssetDto assetDto : getAssetDtoList()) {
            assetMap.put(assetDto.getId(), assetDto);
        }
        return assetMap;
    }

    public List<SubjectDto> getSubjectDtoListNotDeletedCustomerSupplier() {
        List<SubjectDto> allList = getSubjectDtoList();
        List<SubjectDto> filteredList = new ArrayList<>();
        for (SubjectDto subjectDto : allList) {
            if ((subjectDto.getCustomer() || subjectDto.getSupplier())
                    && !Boolean.TRUE.equals(subjectDto.getDeleted())) {
                filteredList.add(subjectDto);
            }
        }
        return filteredList;
    }
}
