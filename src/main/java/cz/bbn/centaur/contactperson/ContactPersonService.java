package cz.bbn.cerberus.contactperson;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.domain.ItemEntity;
import cz.bbn.cerberus.commons.component.ui.factory.ItemFactory;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.CustomPermissionProvider;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactperson.dto.ContactPersonByObjectDto;
import cz.bbn.cerberus.contactperson.dto.ContactPersonByObjectFilterDto;
import cz.bbn.cerberus.contactperson.dto.ContactPersonDto;
import cz.bbn.cerberus.contactperson.dto.ContactPersonFilterDto;
import cz.bbn.cerberus.contactperson.dto.ContactPersonObjectTypeEnum;
import cz.bbn.cerberus.contactperson.dto.TypeByObject;
import cz.bbn.cerberus.contactperson.factory.ContactPersonFactory;
import cz.bbn.cerberus.contactperson.persistance.ContactPersonByObjectDao;
import cz.bbn.cerberus.contactperson.persistance.ContactPersonDao;
import cz.bbn.cerberus.contactperson.persistance.entity.ContactPersonByObjectEntity;
import cz.bbn.cerberus.contactperson.persistance.entity.ContactPersonEntity;
import cz.bbn.cerberus.contactperson.persistance.entity.ContactPersonSimpleEntity;
import cz.bbn.cerberus.contactperson.persistance.repository.ContactPersonByObjectRepository;
import cz.bbn.cerberus.contactperson.persistance.repository.ContactPersonRepository;
import cz.bbn.cerberus.contactperson.persistance.repository.ContactPersonSimpleRepository;
import cz.bbn.cerberus.contactpersontype.ContactPersonTypeService;
import cz.bbn.cerberus.contactpersontype.persistence.ContactPersonTypeEntity;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.custompermission.dto.CustomUserPermissionDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Service
public class ContactPersonService extends CustomPermissionProvider {

    private final ContactPersonRepository contactPersonRepository;
    private final ContactPersonSimpleRepository contactPersonSimpleRepository;
    private final ContactPersonByObjectRepository contactPersonByObjectRepository;
    private final ContactPersonDao contactPersonDao;
    private final ContactPersonByObjectDao contactPersonByObjectDao;
    private final AppLogService appLogService;
    private final ContactPersonTypeService contactPersonTypeService;
    private final CustomPermissionService customPermissionService;
    private final ListService listService;

    public ContactPersonService(ContactPersonRepository contactPersonRepository, ContactPersonDao contactPersonDao,
                                AppLogService appLogService, ContactPersonTypeService contactPersonTypeService,
                                ContactPersonByObjectRepository contactPersonByObjectRepository,
                                ContactPersonSimpleRepository contactPersonSimpleRepository,
                                ContactPersonByObjectDao contactPersonByObjectDao,
                                CustomPermissionService customPermissionService, ListService listService) {
        this.contactPersonRepository = contactPersonRepository;
        this.contactPersonByObjectRepository = contactPersonByObjectRepository;
        this.contactPersonSimpleRepository = contactPersonSimpleRepository;
        this.contactPersonDao = contactPersonDao;
        this.contactPersonByObjectDao = contactPersonByObjectDao;
        this.appLogService = appLogService;
        this.contactPersonTypeService = contactPersonTypeService;
        this.customPermissionService = customPermissionService;
        this.listService = listService;
    }

    public Page<ContactPersonDto> findContactPersonDtoPage(ContactPersonFilterDto filter) {
        return contactPersonDao.findContactPersonPage(filter);
    }

    public Page<ContactPersonDto> findContactPersonExcludedBySubject(ContactPersonFilterDto filterDto,
                                                                     String subjectId) {
        Set<String> excludedIdSet =
                findIdSetByObjectTypeAddedObjectId(ContactPersonObjectTypeEnum.SUBJECT.name(), subjectId);
        return contactPersonDao.findContactPersonXorByIdList(filterDto, excludedIdSet);
    }

    public Page<ContactPersonDto> findContactPersonOnSubjectNotOnObject(ContactPersonFilterDto filterDto,
                                                                        String subjectId, String objectId,
                                                                        String objectType) {
        Set<String> excludedSet = findIdSetByObjectTypeAddedObjectId(objectType, objectId);
        filterDto.setSubjectId(subjectId);
        return contactPersonDao.findContactPersonXorByIdList(filterDto, excludedSet);
    }

    public List<ItemDto> findItemDtoList() {
        Set<String> contactPersonIdSet = SecurityUtils.getCustomReadPermission(
                DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue());
        List<ItemEntity> entityList = contactPersonRepository.findAllAllowedItemList(contactPersonIdSet);
        return ConvertEntities
                .fromEntities(entityList, ItemFactory::fromEntity);
    }

    public ContactPersonDto getContactPerson(String id) throws SystemException {
        ContactPersonEntity entity = getContactPersonEntity(id);
        return ContactPersonFactory.fromEntity(entity);
    }

    public boolean contactPersonExists(String id) {
        return contactPersonRepository.existsById(id);
    }

    public List<String> findObjectIdSetByIdAndObjectType(String objectType, String id) {
        return contactPersonByObjectRepository.findObjectIdSetByIdAndObjectType(objectType, id);
    }

    public Set<String> findIdSetByObjectTypeAddedObjectId(String objectType, String id) {
        return contactPersonByObjectRepository.findIdSetByObjectTypeAddedObjectId(objectType, id);
    }

    @Transactional
    public void changeContactPersonType(String newType, String oldType) throws SystemException {
        contactPersonRepository.changeContactPersonType(new ContactPersonTypeEntity(oldType),
                new ContactPersonTypeEntity(newType));
        contactPersonTypeService.deleteContactPersonType(oldType);
        appLogService.log("change contact person type", "replace contactPersonType for all contact persons from "
                .concat(oldType)
                .concat(" to ")
                .concat(newType), newType);
    }

    @Transactional
    public void saveContactPerson(ContactPersonDto dto) throws SystemException {
        String id = StringUtils.stripAccents(dto.getFirstName()).toUpperCase().concat("_")
                .concat(StringUtils.stripAccents(dto.getLastName()).toUpperCase());
        if (contactPersonRepository.existsById(id)) {
            int sequence = contactPersonRepository.findCountContainId(id);
            id = id.concat("_").concat(String.valueOf(sequence));
        }
        dto.setId(id);
        ContactPersonEntity entity = new ContactPersonEntity();
        id = saveContactPerson(entity, dto);
        listService.reloadContactPersonList();
        customPermissionService.loadPermissionSetForAllUsers();
        appLogService.logInsert(dto, DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue());
        if (dto.getTypeByObjectList() != null) {
            for (TypeByObject typeByObject : dto.getTypeByObjectList()) {
                ContactPersonByObjectDto contactPersonByObjectDto = new ContactPersonByObjectDto();
                contactPersonByObjectDto.setId(id);
                contactPersonByObjectDto.setObjectType(typeByObject.getContactPersonObjectTypeEnum());
                contactPersonByObjectDto.setAddedObjectId(typeByObject.getId());
                addContactPersonByObject(contactPersonByObjectDto);
            }
        }
    }

    @Transactional
    public void getOpportunityContactPersonListAndSaveToContract(ContractDto dto) {
        List<ContactPersonByObjectEntity> contactPersonByObjectEntityList = new ArrayList<>();
        contactPersonByObjectRepository.findByObjectTypeAndAddedToObjectId(
                ContactPersonObjectTypeEnum.OPORTUNITY.name(), dto.getOpportunityDto().getId()
        ).forEach(entity -> {
            ContactPersonByObjectEntity contactPersonByObjectEntity = new ContactPersonByObjectEntity();
            contactPersonByObjectEntity.setContactPerson(entity.getContactPerson());
            contactPersonByObjectEntity.setObjectType(ContactPersonObjectTypeEnum.CONTRACT.name());
            contactPersonByObjectEntity.setAddedToObjectId(dto.getId());
            contactPersonByObjectEntityList.add(contactPersonByObjectEntity);

        });
        contactPersonByObjectRepository.saveAll(contactPersonByObjectEntityList);
    }

    @Transactional
    public void updateContactPerson(ContactPersonDto dto, ContactPersonDto originalDto) throws SystemException {
        ContactPersonEntity entity = getContactPersonEntity(dto.getId());
        this.saveContactPerson(entity, dto);
        listService.reloadContactPersonList();
        appLogService.logUpdate(dto, originalDto, DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void deleteContactPerson(String id) throws SystemException {
        ContactPersonEntity entity = getContactPersonEntity(id);
        entity.setDeleted(true);
        contactPersonByObjectRepository.deleteByContactPersonId(id);
        contactPersonRepository.save(entity);
        listService.reloadContactPersonList();
        appLogService.logDelete(id, DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void deleteAddedContactPerson(String id, ContactPersonObjectTypeEnum addedToType, String addedToId) {
        contactPersonByObjectRepository.delete(id, addedToType.toString(), addedToId);
        appLogService.log("delete contact person from object",
                "object ".concat(addedToType.name()).concat(" with id ").concat(addedToId),
                id);
    }

    @Transactional
    public void addContactPersonByObject(ContactPersonByObjectDto dto) throws SystemException {
        ContactPersonByObjectEntity entity = convertContactByObjectToEntity(dto);
        contactPersonByObjectRepository.save(entity);
        appLogService.log("add contact person to object",
                "object ".concat(dto.getObjectType().name()).concat(" to id ").concat(dto.getAddedObjectId()),
                dto.getId());
    }

    @Transactional
    public void addContactPersonListByObject(Set<ContactPersonByObjectDto> dtoSet) throws SystemException {
        Set<ContactPersonByObjectEntity> entitySet = new HashSet<>();
        for (ContactPersonByObjectDto dto : dtoSet) {
            entitySet.add(convertContactByObjectToEntity(dto));
        }
        contactPersonByObjectRepository.saveAll(entitySet);
        appLogService.logInsertMultiple(dtoSet, DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void addContactPersonToSubject(SubjectDto subjectDto, String contactPersonId) throws SystemException {
        ContactPersonByObjectDto contactPersonByObjectDto = new ContactPersonByObjectDto();
        contactPersonByObjectDto.setId(contactPersonId);
        contactPersonByObjectDto.setObjectType(ContactPersonObjectTypeEnum.SUBJECT);
        contactPersonByObjectDto.setAddedObjectId(subjectDto.getId());
        addContactPersonByObject(contactPersonByObjectDto);
    }

    public List<String> getUsedContactPersonType(String id) {
        return contactPersonRepository.getUsedContactPersonTypeList(id);
    }

    private ContactPersonByObjectEntity convertContactByObjectToEntity(ContactPersonByObjectDto dto)
            throws SystemException {
        ContactPersonByObjectEntity entity = new ContactPersonByObjectEntity();
        entity.setAddedToObjectId(dto.getAddedObjectId());
        entity.setObjectType(dto.getObjectType().toString());
        entity.setContactPerson(getContactPersonSimpleEntity(dto.getId()));
        return entity;
    }

    private String saveContactPerson(ContactPersonEntity entity, ContactPersonDto dto) {
        ContactPersonFactory.fillEntity(entity, dto);
        String id = contactPersonRepository.save(entity).getId();

        CustomUserPermissionDto customUserPermissionDto = new CustomUserPermissionDto(
                DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue(), CustomPermissionService.ALL_PERMISSION,
                SecurityUtils.getCurrentUserId(), dto.getId(), true
        );
        customPermissionService.saveSinglePermission(customUserPermissionDto);
        appLogService.logInsert(customUserPermissionDto, CustomPermissionService.CUSTOM_PERMISSION_OBJECT_NAME);
        return id;
    }

    private ContactPersonEntity getContactPersonEntity(String id) throws SystemException {
        return contactPersonRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.CONTACT_PERSON_NOT_EXISTS, id));
    }

    private ContactPersonSimpleEntity getContactPersonSimpleEntity(String id) throws SystemException {
        return contactPersonSimpleRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.CONTACT_PERSON_NOT_EXISTS, id));
    }

    public Page<ContactPersonByObjectDto> findSubjectContactPageByObjectPage(ContactPersonByObjectFilterDto filterDto) {
        return contactPersonByObjectDao.findContactPersonByObjectPage(filterDto);
    }

    public List<String> findNotUsedByObjectByUser(ContactPersonObjectTypeEnum objectType, String objectId) {
        Set<String> contactPersonSet = contactPersonDao.getAllowedContactPerson();
        return contactPersonRepository.findNotUsedByObjectByUser(objectType.toString(), objectId, contactPersonSet);
    }

    @Override
    protected List<String> findAllId() {
        return contactPersonRepository.findAllIdList();
    }

    @Override
    public Set<DomainEnum> getDomainSet() {
        return EnumSet.of(DomainEnum.CONTACT_PERSON_DOMAIN_NAME);
    }

    @Override
    protected boolean showInCustomPermissions() {
        return true;
    }

    @Override
    public Map<String, Long> getOwnerMap() {
        return new HashMap<>();
    }
}
