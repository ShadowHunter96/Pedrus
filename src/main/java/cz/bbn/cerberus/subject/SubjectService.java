package cz.bbn.cerberus.subject;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.ares.AresService;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.domain.ItemEntity;
import cz.bbn.cerberus.commons.component.ui.factory.ItemFactory;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.CustomPermissionProvider;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.ui.ContractSalesDetailView;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.custompermission.dto.CustomUserPermissionDto;
import cz.bbn.cerberus.ico.dto.IcoDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.notification.NotificationService;
import cz.bbn.cerberus.opportunity.ui.OpportunityDetailView;
import cz.bbn.cerberus.project.ui.ProjectDetailView;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.subject.dto.SubjectFilterDto;
import cz.bbn.cerberus.subject.factory.SubjectFactory;
import cz.bbn.cerberus.subject.persistance.SubjectByObjectEntity;
import cz.bbn.cerberus.subject.persistance.SubjectByObjectRepository;
import cz.bbn.cerberus.subject.persistance.SubjectDao;
import cz.bbn.cerberus.subject.persistance.SubjectEntity;
import cz.bbn.cerberus.subject.persistance.SubjectObjectId;
import cz.bbn.cerberus.subject.persistance.SubjectRepository;
import cz.bbn.cerberus.subject.ui.SubjectDetailView;
import cz.bbn.cerberus.suppliertype.SupplierTypeService;
import cz.bbn.cerberus.suppliertype.persistance.SupplierTypeEntity;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.usermessage.MessageType;
import cz.bbn.cerberus.usermessage.UserMessageObjectType;
import cz.bbn.cerberus.usermessage.UserMessageService;
import cz.bbn.cerberus.usermessage.dto.UserMessageDto;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

@Service
@Slf4j
public class SubjectService extends CustomPermissionProvider {

    private final SubjectDao subjectDao;
    private final SubjectRepository subjectRepository;
    private final CustomPermissionService customPermissionService;
    private final AppLogService appLogService;
    private final ListService listService;
    private final UserMessageService userMessageService;
    private final SubjectByObjectRepository subjectByObjectRepository;
    private final SupplierTypeService supplierTypeService;
    private final NotificationService notificationService;
    private final AresService aresService;
    private final AppEnv appEnv;

    public SubjectService(SubjectDao subjectDao, SubjectRepository subjectRepository,
                          CustomPermissionService customPermissionService, AppLogService appLogService,
                          ListService listService, UserMessageService userMessageService,
                          SubjectByObjectRepository subjectByObjectRepository,
                          SupplierTypeService supplierTypeService, NotificationService notificationService,
                          AresService aresService, AppEnv appEnv) {
        this.subjectDao = subjectDao;
        this.subjectRepository = subjectRepository;
        this.customPermissionService = customPermissionService;
        this.appLogService = appLogService;
        this.listService = listService;
        this.userMessageService = userMessageService;
        this.subjectByObjectRepository = subjectByObjectRepository;
        this.supplierTypeService = supplierTypeService;
        this.notificationService = notificationService;
        this.aresService = aresService;
        this.appEnv = appEnv;
    }

    public Page<SubjectDto> findSubjectPage(SubjectFilterDto subjectFilterDto) {
        return subjectDao.findSubjectPage(subjectFilterDto);
    }

    public Long getCountByUser(Long userId) {
        return listService.getSubjectDtoListByCustomer().stream()
                .filter(subjectDto -> subjectDto.getUserDto().getId().equals(userId)).count();
    }

    public List<SubjectDto> findSubjectAllowedList() {
        return subjectDao.findSubjectAllowedListByUser();
    }

    public List<ItemDto> findItemDtoList() {
        Set<String> subjectIdSet = SecurityUtils.getCustomReadPermission(DomainEnum.SUBJECT_DOMAIN_NAME.getValue());
        List<ItemEntity> entityList = subjectRepository.findAllAllowedItemList(subjectIdSet);
        return ConvertEntities
                .fromEntities(entityList, ItemFactory::fromEntity);
    }

    public SubjectDto getSubjectDto(String id) throws SystemException {
        return SubjectFactory.fromEntity(getEntityById(id));
    }

    @Transactional
    public void updateSubject(SubjectDto dto, SubjectDto originalDto) throws SystemException {
        if (dto.getId() == null) {
            throw new SystemException(ErrorCode.SUBJECT_NOT_EXISTS);
        }
        SubjectEntity entity = getEntityById(dto.getId());
        saveSubjectEntity(entity, dto);
        listService.reloadSubjectDtoList();

        for (UserMessageDto messageDto : generateUserMessageDto(dto, originalDto)) {
            userMessageService.saveUserMessage(messageDto);
        }
        appLogService.logUpdate(dto, originalDto, DomainEnum.SUBJECT_DOMAIN_NAME.getValue());
        customPermissionService.loadPermissionSetByCurrentUser();
    }

    @Transactional
    public void saveSubject(SubjectDto dto) throws SystemException {
        if (subjectExists(dto.getId())) {
            throw new SystemException(ErrorCode.SUBJECT_ALREADY_EXISTS, dto.getId());
        }
        SubjectEntity entity = new SubjectEntity();
        if (Boolean.TRUE.equals(dto.getLocalSubject())) {
            dto.setLastUpdateFromAres(LocalDateTime.now());
        }
        saveSubjectEntity(entity, dto);
        listService.reloadSubjectDtoList();
        CustomUserPermissionDto customUserPermissionDto = new CustomUserPermissionDto(
                DomainEnum.SUBJECT_DOMAIN_NAME.getValue(), CustomPermissionService.ALL_PERMISSION,
                dto.getUserDto().getId(), dto.getId(), true
        );

        customPermissionService.saveSinglePermission(customUserPermissionDto);
        appLogService.logInsert(customUserPermissionDto, CustomPermissionService.CUSTOM_PERMISSION_OBJECT_NAME);
        appLogService.logInsert(dto, DomainEnum.SUBJECT_DOMAIN_NAME.getValue());
        customPermissionService.loadPermissionSetByCurrentUser();
    }

    @Transactional
    public void deleteSubject(String id) throws SystemException {
        SubjectEntity entity = getEntityById(id);
        entity.setDeleted(true);
        subjectRepository.save(entity);
        listService.reloadSubjectDtoList();
        appLogService.logDelete(id, DomainEnum.SUBJECT_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void updateOwner(Long userId, String id){
        subjectRepository.updateOwner(userId, id);
    }

    @Transactional
    public void updateOwner(Long ownerId, Long newOwnerId){
        subjectRepository.updateOwner(ownerId, newOwnerId);
    }

    public List<SubjectDto> getSubjectByContactPerson(String contactPersonId) {
        return subjectDao.getSubjectByContactPerson(contactPersonId);
    }

    public void linkSubject(Set<SubjectDto> subjectDtoSet, String objectId, ObjectType objectType) {
        List<SubjectByObjectEntity> subjectByObjectEntityList = new ArrayList<>();
        subjectDtoSet.forEach(subjectDto -> {
            SubjectByObjectEntity subjectByObjectEntity = new SubjectByObjectEntity();
            subjectByObjectEntity.setId(new SubjectObjectId(subjectDto.getId(), objectId, objectType));
            subjectByObjectEntityList.add(subjectByObjectEntity);
            generateNotificationEntity(subjectDto, objectId, objectType);
            appLogService.log("Link subject to object", subjectByObjectEntity.toString(), objectId);
        });
        subjectByObjectRepository.saveAll(subjectByObjectEntityList);
    }

    public Page<SubjectDto> getSubjectByObjectPage(String objectId, ObjectType objectType, int page, int size) {
        return subjectDao.findSubjectPage(objectId, objectType, page, size);
    }

    public void deleteSubjectByObject(String subjectId, String objectId, ObjectType objectType) {
        subjectByObjectRepository.delete(
                new SubjectByObjectEntity(new SubjectObjectId(subjectId, objectId, objectType)));
    }

    public List<String> getIdListBySupplierTypeId(String typeId) {
        return subjectRepository.getIdListBySupplierTypeId(typeId);
    }

    @Transactional
    public void changeSupplierType(String newType, String oldType) throws SystemException {
        subjectRepository.changeSupplierType(new SupplierTypeEntity(newType), new SupplierTypeEntity(oldType));
        supplierTypeService.deleteSupplierType(oldType);
        appLogService.log("change supplier  type", "replace supplier type for all documents from "
                .concat(oldType)
                .concat(" to ")
                .concat(newType), newType);
    }

    public List<String> getIdByIcoOrDic(String ico, String dic) {
        if (ico == null || StringUtils.isEmpty(ico)) {
            return new ArrayList<>();
        }
        if (dic != null && !StringUtils.isEmpty(dic)) {
            return subjectRepository.findByIcoOrDic(ico, dic);
        }
        return subjectRepository.findIdByIco(ico);
    }

    public void updateChungFromAres(int numberOfSub) {
        List<SubjectDto> subjectList = ConvertEntities.fromEntities(
                subjectRepository.getListFromAresUpdate(numberOfSub), SubjectFactory::fromEntity);
        List<SubjectDto> updatedSubjectList = new ArrayList<>();
        for (SubjectDto subjectDto : subjectList) {
            SubjectDto updatedDto = updateDataFromAres(subjectDto);
            updatedDto.setLastUpdateFromAres(LocalDateTime.now());
            appLogService.logUpdate(updatedDto, subjectDto, DomainEnum.SUPPLIER_DOMAIN_NAME.getValue());
            updatedSubjectList.add(updatedDto);
        }
        List<SubjectEntity> entityList = new ArrayList<>();
        for (SubjectDto dto : updatedSubjectList) {
            SubjectEntity entity = new SubjectEntity();
            SubjectFactory.fillEntity(entity, dto);
            entityList.add(entity);
        }
        subjectRepository.saveAll(entityList);

    }

    public SubjectDto updateDataFromAres(SubjectDto subjectDto) {
        IcoDto icoDto = setSubjectDataToIco(subjectDto);
        try {
            icoDto = aresService.getAnswerFromEs(icoDto);
            icoDto = aresService.getDataFromAres(icoDto);
            icoDto = aresService.getSingleResponseFromAdis(icoDto);
            subjectDto = setIcoDataToSubject(subjectDto, icoDto);
        } catch (SystemException e) {
            log.error(TextValues.SYSTEM_EXCEPTION, e);
            ErrorNotification.show(e, appEnv);
        }
        return subjectDto;
    }

    public SubjectDto setIcoDataToSubject(SubjectDto subjectDto, IcoDto icoDto) {

        subjectDto.setCourt(icoDto.getCourt());
        subjectDto.setFileNumber(icoDto.getFileNumber());
        subjectDto.setRegister(icoDto.getRegister());

        subjectDto.setCompanyName(icoDto.getCompanyName());
        subjectDto.setIco(icoDto.getIco());
        subjectDto.setLawForm(icoDto.getLawForm());
        subjectDto.setAddress(icoDto.getAddress());
        subjectDto.setEnlistDate(icoDto.getEnlistDate());

        subjectDto.setCapital(icoDto.getCapital());
        if (icoDto.getCapital() != null && !"".equals(icoDto.getCapital().trim().replaceAll("[^\\d.]", ""))) {
            subjectDto.setCapitalDecimal(BigDecimal.valueOf(Double.parseDouble(icoDto.getCapital().trim()
                    .replace(",", ".").replaceAll("[^\\d.]", ""))));
        }

        subjectDto.setDic(icoDto.getDic());
        subjectDto.setReliable(icoDto.getReliable());
        subjectDto.setUnreliableFrom(icoDto.getUnreliableFrom());
        subjectDto.setStandardAccount(icoDto.getStandardAccount());
        subjectDto.setNonStandardAccount(icoDto.getNonStandardAccount());

        subjectDto.setCompanions(icoDto.getCompanionNames());
        return subjectDto;
    }

    private IcoDto setSubjectDataToIco(SubjectDto subjectDto) {
        IcoDto icoDto = new IcoDto();

        icoDto.setCourt(subjectDto.getCourt());
        icoDto.setFileNumber(subjectDto.getFileNumber());
        icoDto.setRegister(subjectDto.getRegister());

        icoDto.setCompanyName(subjectDto.getCompanyName());
        icoDto.setIco(subjectDto.getIco());
        icoDto.setLawForm(subjectDto.getLawForm());
        icoDto.setAddress(subjectDto.getAddress());
        icoDto.setEnlistDate(subjectDto.getEnlistDate());

        icoDto.setCapital(subjectDto.getCapital());

        icoDto.setDic(subjectDto.getDic());
        icoDto.setReliable(subjectDto.getReliable());
        icoDto.setUnreliableFrom(subjectDto.getUnreliableFrom());
        icoDto.setStandardAccount(subjectDto.getStandardAccount());
        icoDto.setNonStandardAccount(subjectDto.getNonStandardAccount());

        icoDto.setCompanionNames(subjectDto.getCompanions());
        return icoDto;
    }

    private void saveSubjectEntity(SubjectEntity entity, SubjectDto dto) throws SystemException {
        if (Boolean.FALSE.equals(dto.getCustomer())
                && Boolean.FALSE.equals(dto.getSupplier())
                && Boolean.FALSE.equals(dto.getOwnCompany())) {
            throw new SystemException(ErrorCode.SUBJECT_CHOOSE_ONE_BOOLEAN);
        }
        SubjectFactory.fillEntity(entity, dto);
        if (entity.getDeleted() == null) {
            entity.setDeleted(false);
        }
        subjectRepository.save(entity);
    }

    private List<UserMessageDto> generateUserMessageDto(SubjectDto dto, SubjectDto originalDto) {
        String message = createMessage(dto, originalDto);
        List<UserMessageDto> messageList = new ArrayList<>();
        if (!"".equals(message.trim())) {
            for (Long userId : customPermissionService
                    .getUserNameSetByObjectIdObjectType(dto.getId(), DomainEnum.SUBJECT_DOMAIN_NAME.getValue())) {
                UserMessageDto userMessageDto = new UserMessageDto();
                userMessageDto.setObjectId(dto.getId());
                userMessageDto.setObjectType(UserMessageObjectType.SUBJECT);
                userMessageDto.setDueDate(LocalDateTime.now().plusDays(20));
                userMessageDto.setPriority(false);
                userMessageDto.setType(MessageType.SUBJECT_CHANGE);
                userMessageDto.setMessage(message);
                userMessageDto.setUserId(userId);
                userMessageDto.setViewed(false);
                messageList.add(userMessageDto);
            }
        }
        return messageList;
    }

    private String createMessage(SubjectDto dto, SubjectDto originalDto) {
        StringBuilder builder = new StringBuilder();
        checkParam("ICO changed.", dto.getIco(), originalDto.getIco(), builder);
        checkParam("Name changed.", dto.getName(), originalDto.getName(), builder);
        checkParam("Court changed.", dto.getCourt(), originalDto.getCourt(), builder);
        checkParam("File number changed.", dto.getFileNumber(), originalDto.getFileNumber(), builder);
        checkParam("Register changed.", dto.getRegister(), originalDto.getRegister(), builder);
        checkParam("Company name changed.", dto.getCompanyName(), originalDto.getCompanyName(), builder);
        checkParam("Legal form changed.", dto.getLawForm(), originalDto.getLawForm(), builder);
        checkParam("Address changed.", dto.getAddress(), originalDto.getAddress(), builder);
        checkParam("Registration date changed.", dto.getEnlistDate(), originalDto.getEnlistDate(), builder);
        checkParam("Capital changed.", dto.getCapital(), originalDto.getCapital(), builder);
        checkParam("Companions changed.", dto.getCompanions(), originalDto.getCompanions(), builder);
        checkParam("URL changed.", dto.getUrl(), originalDto.getUrl(), builder);
        checkParam("Description changed.", dto.getDescription(), originalDto.getDescription(), builder);
        return builder.toString();
    }

    private void checkParam(String text, String newValue, String oldValue, StringBuilder builder) {
        if (!Objects.equals(newValue, oldValue)) {
            builder.append(text).append("\n").append("New value: ").append(newValue).append(" Old value: ")
                    .append(oldValue).append("\n\n");
        }
    }

    public boolean subjectExists(String id) {
        return subjectRepository.findById(id).isPresent();
    }

    private SubjectEntity getEntityById(String id) throws SystemException {
        return subjectRepository.findById(id)
                .orElseThrow(() -> new SystemException(ErrorCode.SUBJECT_NOT_EXISTS, id));
    }

    public Map<ItemDto, Double> getCustomersMapByUser(List<UserDto> userDtoList) {
        Map<ItemDto, Double> actualMap = AppUtils.getUserMapWithDouble(userDtoList);
        listService.getSubjectDtoList().stream().forEach(subjectDto -> {
            if (!Boolean.TRUE.equals(subjectDto.getDeleted()) && Boolean.TRUE.equals(subjectDto.getCustomer())
                    && subjectDto.getUserDto() != null) {
                UserDto userDto = subjectDto.getUserDto();
                Double count = 0D;
                if(actualMap.containsKey(new ItemDto(userDto))) {
                    count = actualMap.get(new ItemDto(userDto)) + 1;
                }
                actualMap.put(new ItemDto(userDto), count);
            }
        });

        return AppUtils.removeZeroAndGetMap(actualMap);
    }

    public List<SubjectDto> getAllowedCustomers() {
        return subjectDao.getAllowedCustomers();
    }

    @Override
    public List<String> findAllId() {
        return subjectRepository.findAllId();
    }

    @Override
    public Set<DomainEnum> getDomainSet() {
        return EnumSet.of(DomainEnum.SUBJECT_DOMAIN_NAME);
    }

    @Override
    protected boolean showInCustomPermissions() {
        return true;
    }

    @Override
    public Map<String, Long> getOwnerMap() {
        Map<String, Long> ownerMap = new HashMap<>();
        for (SubjectDto dto : listService.getSubjectDtoList()) {
            if (dto.getUserDto() != null) {
                ownerMap.put(dto.getId(), dto.getUserDto().getId());
            }
        }
        return ownerMap;
    }

    public List<SubjectDto> getMySubjects() {
        return subjectDao.getMySubjects();
    }

    private void generateNotificationEntity(SubjectDto subjectDto, String objectId, ObjectType objectType) {
        UserDto userDto = subjectDto.getUserDto();
        String language = Optional.ofNullable(userDto.getPreferredLanguage()).orElse(Transl.DEFAULT_LANG);
        String route;
        switch (objectType) {
            case OPPORTUNITY -> {
                route = OpportunityDetailView.ROUTE;
                objectId = objectId.replace("/", "&ndash");
            }
            case CONTRACT -> route = ContractSalesDetailView.ROUTE;
            case PROJECT -> route = ProjectDetailView.ROUTE;
            default -> route = "";
        }
        if (!userDto.getId().equals(SecurityUtils.getCurrentUserId())) {
            String message = Transl.getByLang("User", language).concat(" ").concat(SecurityUtils.getCurrentUserName()).concat(" ")
                    .concat(Transl.getByLang("linked supplier", language).concat(" ")
                            .concat(AppUtils.generateUrl(appEnv.getProjectUrl(), SubjectDetailView.ROUTE, subjectDto.getId(), subjectDto.getName())).concat(" ")
                            .concat(Transl.getByLang("on entity", language)).concat(" ").concat(Transl.get(objectType.name())).concat(" ")
                            .concat(AppUtils.generateUrl(appEnv.getProjectUrl(), route, objectId, objectId)));
            notificationService.saveEmailLowNotification(MessageType.SUPPLIER_LINKED.name(), message, userDto.getId());
        }
    }

    public String getOwnCompanyName() {
        List<String> ownCompanyList = subjectRepository.getOwnCompanyNameList();
        if (ownCompanyList != null && !ownCompanyList.isEmpty()) {
            return StringUtils.join(ownCompanyList, ", ");
        }
        return "";
    }
}
