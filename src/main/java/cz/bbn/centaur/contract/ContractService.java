package cz.bbn.cerberus.contract;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.domain.ItemEntity;
import cz.bbn.cerberus.commons.component.ui.factory.ItemFactory;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.CustomPermissionProvider;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactperson.ContactPersonService;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.contract.dto.ContractFilterDto;
import cz.bbn.cerberus.contract.dto.ContractInternalType;
import cz.bbn.cerberus.contract.factory.ContractFactory;
import cz.bbn.cerberus.contract.persistence.entity.ContractEntity;
import cz.bbn.cerberus.contract.persistence.repository.ContractDao;
import cz.bbn.cerberus.contract.persistence.repository.ContractRepository;
import cz.bbn.cerberus.contract.ui.ContractSalesDetailView;
import cz.bbn.cerberus.contracttype.ContractTypeService;
import cz.bbn.cerberus.contracttype.persistence.ContractTypeEntity;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.custompermission.dto.CustomUserPermissionDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.notification.NotificationService;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.subject.ui.SubjectDetailView;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.usermessage.MessageType;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.time.LocalDate;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

@Service
public class ContractService extends CustomPermissionProvider {

    private final ContractDao contractDao;
    private final ContractRepository contractRepository;
    private final AppLogService appLogService;
    private final CustomPermissionService customPermissionService;
    private final ListService listService;
    private final ContactPersonService contactPersonService;
    private final ContractTypeService contractTypeService;
    private final NotificationService notificationService;
    private final AppEnv appEnv;

    public ContractService(ContractDao contractDao, ContractRepository contractRepository,
                           AppLogService appLogService, CustomPermissionService customPermissionService,
                           ListService listService, ContactPersonService contactPersonService,
                           ContractTypeService contractTypeService, NotificationService notificationService,
                           AppEnv appEnv) {
        this.contractDao = contractDao;
        this.contractRepository = contractRepository;
        this.appLogService = appLogService;
        this.customPermissionService = customPermissionService;
        this.listService = listService;
        this.contactPersonService = contactPersonService;
        this.contractTypeService = contractTypeService;
        this.notificationService = notificationService;
        this.appEnv = appEnv;
    }

    public Page<ContractDto> findContractDtoPage(ContractFilterDto filter) {
        return contractDao.findContractPage(filter);
    }

    public List<ItemDto> findItemDtoList() {
        Set<String> allowedId = SecurityUtils.getCustomReadPermission(DomainEnum.CONTRACT_DOMAIN_NAME.getValue());
        List<ItemEntity> entityList = contractRepository.findAllowedItemList(allowedId);
        return ConvertEntities
                .fromEntities(entityList, ItemFactory::fromEntity);
    }

    public List<ContractDto> getContractListBySubject(String subject) {
        List<ContractEntity> contractEntityList = contractRepository.getContractListBySubject(subject);
        return ConvertEntities
                .fromEntities(contractEntityList, ContractFactory::fromEntity);
    }

    public ContractDto getContract(String id) throws SystemException {
        ContractEntity entity = getEntityById(id);
        return ContractFactory.fromEntity(entity);
    }

    public List<ContractDto> getMyInvoiceEditContractList() {
        return contractDao.getMyInvoiceEditContractList();
    }

    public Long getContractCountForDashboard(LocalDate from, LocalDate to) {
        Set<String> objectIdSet = SecurityUtils.getAllowedEntityIdByDomain(
                Permission.CONTRACT_EDIT.name(), DomainEnum.CONTRACT_DOMAIN_NAME.getValue());
        return listService.getContractList()
                .stream()
                .filter(contractDto -> {
                    LocalDate date = contractDto.getValidityStart();
                    return date != null && (date.isAfter(from) || date.isEqual(from)) && (date.isBefore(to)
                            || date.isEqual(to)) && objectIdSet.contains(contractDto.getId());
                }).count();
    }

    public Map<ItemDto, Double> getContractsMapByUser(List<UserDto> userDtoList, LocalDate from, LocalDate to) {
        Map<ItemDto, Double> actualMap = AppUtils.getUserMapWithDouble(userDtoList);
        listService.getContractList().stream().forEach(contractDto -> {
            LocalDate date = contractDto.getValidityStart();
            if (!Boolean.TRUE.equals(contractDto.getDeleted())
                    && date != null && (date.isAfter(from) || date.isEqual(from))
                    && (date.isBefore(to) || date.isEqual(to))
                    && contractDto.getUserDto() != null) {
                UserDto userDto = contractDto.getUserDto();
                Double count = actualMap.get(new ItemDto(userDto)) + 1;
                actualMap.put(new ItemDto(userDto), count);
            }
        });
        return AppUtils.removeZeroAndGetMap(actualMap);
    }

    @Transactional
    public void saveContractFromOpportunity(ContractDto dto) throws SystemException {
        saveContract(dto);
        contactPersonService.getOpportunityContactPersonListAndSaveToContract(dto);
    }

    @Transactional
    public void saveContract(ContractDto dto) throws SystemException {
        List<Integer> sequenceList = contractRepository.getSequenceList();
        int sequence = 0;
        if (sequenceList.isEmpty() || sequenceList.get(0) == null) {
            sequence++;
        } else {
            sequence = sequenceList.get(0) + 1;
        }

        ContractEntity entity = new ContractEntity();

        String id = "";

        if (dto.getInternalType() == ContractInternalType.SALES) {
            id = AppUtils.generateIdSuffix("CS", sequence, appEnv.getContractSuffix());
        }
        if (dto.getInternalType() == ContractInternalType.SUPPLIER) {
            id = AppUtils.generateId("CP", sequence);
        }
        if (dto.getInternalType() == ContractInternalType.OPERATIONAL) {
            id = AppUtils.generateId("CO", sequence);
        }

        if (contractRepository.existsById(id)) {
            throw new SystemException(ErrorCode.CONTRACT_ALREADY_EXISTS, id);
        }

        if (dto.getType() != null && Boolean.TRUE.equals(dto.getType().getConnectionRequired())) {
            List<Integer> subSequenceList = contractRepository.getSubSequence(dto.getConnectedContract().getId());
            int subsequence = 0;
            if (subSequenceList.isEmpty() || subSequenceList.get(0) == null) {
                subsequence++;
            } else {
                subsequence = subSequenceList.get(0) + 1;
            }
            entity.setSubsequence(subsequence);
            id = AppUtils.generateIdAddition(dto.getConnectedContract().getId(), subsequence);
        } else {
            entity.setSequence(sequence);
        }

        dto.setId(id);

        if (dto.getAddition() == null) {
            dto.setAddition(false);
        }

        saveContract(entity, dto);

        CustomUserPermissionDto customUserPermissionDto = new CustomUserPermissionDto(
                DomainEnum.CONTRACT_DOMAIN_NAME.getValue(), CustomPermissionService.ALL_PERMISSION,
                SecurityUtils.getCurrentUserId(), dto.getId(), true
        );
        customPermissionService.saveSinglePermission(customUserPermissionDto);
        generateNotificationEntity(dto);
        appLogService.logInsert(customUserPermissionDto, CustomPermissionService.CUSTOM_PERMISSION_OBJECT_NAME);
        appLogService.logInsert(dto, DomainEnum.CONTRACT_DOMAIN_NAME.getValue());
        listService.reloadContractList();
        customPermissionService.loadPermissionSetByCurrentUser();
    }


    @Transactional
    public void updateContract(ContractDto dto, ContractDto originalDto) throws SystemException {
        ContractEntity entity = getEntityById(dto.getId());
        saveContract(entity, dto);
        listService.reloadContractList();
        appLogService.logUpdate(dto, originalDto, DomainEnum.CONTRACT_DOMAIN_NAME.getValue());
        customPermissionService.loadPermissionSetByCurrentUser();
    }

    @Transactional
    public void deleteContract(String id) throws SystemException {
        ContractEntity entity = getEntityById(id);
        entity.setDeleted(true);
        contractRepository.save(entity);
        listService.reloadContractList();
        appLogService.logDelete(id, DomainEnum.CONTRACT_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void updateOwner(Long userId, String id){
        contractRepository.updateOwner(userId, id);
    }
    @Transactional
    public void updateOwner(Long ownerId, Long newOwnerId){
        contractRepository.updateOwner(ownerId, newOwnerId);
    }

    private void saveContract(ContractEntity entity, ContractDto dto) throws SystemException {
        if (dto.getConnectedContract() != null) {
            if (dto.getConnectedContract().getId().equals(dto.getId())) {
                throw new SystemException(ErrorCode.ITSELF_CONNECT_ERROR, dto.getId());
            }
            if (dto.getConnectedContract().getSubjectDto() != null && dto.getSubjectDto() != null &&
                    !dto.getConnectedContract().getSubjectDto().getId().equals(dto.getSubjectDto().getId())) {
                throw new SystemException(ErrorCode.CONNECTED_CONTRACT_BAD_SUBJECT_ERROR);
            }
        }
        ContractFactory.fillEntity(entity, dto);
        contractRepository.save(entity);
    }

    private ContractEntity getEntityById(String id) throws SystemException {
        return contractRepository.findById(id)
                .orElseThrow(() -> new SystemException(ErrorCode.CONTRACT_NOT_EXISTS, id));
    }

    @Override
    protected List<String> findAllId() {
        return contractRepository.findAllId();
    }

    @Override
    public Set<DomainEnum> getDomainSet() {
        return EnumSet.of(DomainEnum.CONTRACT_DOMAIN_NAME, DomainEnum.INVOICE_DOMAIN_NAME);
    }

    @Override
    protected boolean showInCustomPermissions() {
        return true;
    }

    @Override
    public Map<String, Long> getOwnerMap() {
        Map<String, Long> ownerMap = new HashMap<>();
        for (ContractDto dto : listService.getContractList()) {
            if (dto.getUserDto() != null) {
                ownerMap.put(dto.getId(), dto.getUserDto().getId());
            }
        }
        return ownerMap;
    }

    public List<String> getUsedContractType(String id) {
        return contractRepository.getUsedContractTypeList(id);
    }

    @Transactional
    public void changeContractType(String newType, String oldType) throws SystemException {
        contractRepository.changeContractType(new ContractTypeEntity(oldType),
                new ContractTypeEntity(newType));
        contractTypeService.deleteContractType(oldType);
        appLogService.log("change contract type", "replace contractType for all contracts from "
                .concat(oldType)
                .concat(" to ")
                .concat(newType), newType);
    }

    private void generateNotificationEntity(ContractDto contractDto) {
        SubjectDto subjectDto = contractDto.getSubjectDto();
        UserDto userDto = subjectDto.getUserDto();
        String language = Optional.ofNullable(userDto.getPreferredLanguage()).orElse(Transl.DEFAULT_LANG);

        if (!userDto.getId().equals(SecurityUtils.getCurrentUserId())) {
            String message = Transl.getByLang("User", language).concat(" ").concat(SecurityUtils.getCurrentUserName()).concat(" ")
                    .concat(Transl.getByLang("created a new contract", language).concat(" ")
                            .concat(AppUtils.generateUrl(appEnv.getProjectUrl(), ContractSalesDetailView.ROUTE, contractDto.getId(), contractDto.getName())).concat(" ")
                            .concat(Transl.getByLang("on subject", language)).concat(" ")
                            .concat(AppUtils.generateUrl(appEnv.getProjectUrl(), SubjectDetailView.ROUTE, subjectDto.getId(), subjectDto.getName())));
            notificationService.saveEmailLowNotification(MessageType.NEW_CONTRACT.name(), message, userDto.getId());
        }
    }
}
