package cz.bbn.cerberus.contactpersontype;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.contactpersontype.dto.ContactPersonTypeDto;
import cz.bbn.cerberus.contactpersontype.factory.ContactPersonTypeFactory;
import cz.bbn.cerberus.contactpersontype.persistence.ContactPersonTypeEntity;
import cz.bbn.cerberus.contactpersontype.persistence.ContactPersonTypeRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;

@Service
public class ContactPersonTypeService {

    private final ContactPersonTypeRepository contactPersonTypeRepository;
    private final AppLogService appLogService;

    public ContactPersonTypeService(ContactPersonTypeRepository contactPersonTypeRepository,
                                    AppLogService appLogService) {
        this.contactPersonTypeRepository = contactPersonTypeRepository;
        this.appLogService = appLogService;
    }

    public Page<ContactPersonTypeDto> findContactPersonTypeDtoPage(int page, int size, List<Sort.Order> sortList) {
        Page<ContactPersonTypeEntity> pageEntity =
                contactPersonTypeRepository.findAll(PageRequest.of(page, size, Sort.by(sortList)));
        List<ContactPersonTypeDto> list = ConvertEntities
                .fromEntities(pageEntity.toList(), ContactPersonTypeFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(page, size, Sort.by(sortList)), pageEntity.getTotalElements());
    }

    public List<ContactPersonTypeDto> findAllEnabled(String exceptId) {
        List<ContactPersonTypeEntity> entityList = contactPersonTypeRepository.findAllEnabled(exceptId);
        return ConvertEntities
                .fromEntities(entityList, ContactPersonTypeFactory::fromEntity);
    }

    public List<ContactPersonTypeDto> findAllEnabled() {
        List<ContactPersonTypeEntity> entityList = contactPersonTypeRepository.findAllEnabled();
        return ConvertEntities
                .fromEntities(entityList, ContactPersonTypeFactory::fromEntity);
    }

    public List<ContactPersonTypeDto> findAll() {
        List<ContactPersonTypeEntity> entityList = contactPersonTypeRepository.findAll();
        return ConvertEntities
                .fromEntities(entityList, ContactPersonTypeFactory::fromEntity);
    }

    public boolean isContactPersonTypeAllowed(String id) {
        return contactPersonTypeRepository.isContactPersonTypeAllowed(id);
    }

    public ContactPersonTypeDto getContactPersonType(String id) throws SystemException {
        ContactPersonTypeEntity entity = getContactPersonTypeEntity(id);
        return ContactPersonTypeFactory.fromEntity(entity);
    }

    public boolean contactPersonTypeExists(String id) {
        return contactPersonTypeRepository.existsById(id);
    }

    @Transactional
    public void saveContactPersonType(ContactPersonTypeDto dto) throws SystemException {
        if (contactPersonTypeExists(dto.getId())) {
            throw new SystemException(ErrorCode.ID_ALREADY_EXISTS, dto.getId());
        }
        ContactPersonTypeEntity entity = new ContactPersonTypeEntity();
        saveContactPersonType(entity, dto);
        appLogService.logInsert(dto, DomainEnum.CONTACT_PERSON_TYPE_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void updateContactTypePerson(ContactPersonTypeDto dto,
                                        ContactPersonTypeDto originalDto) throws SystemException {
        ContactPersonTypeEntity entity = getContactPersonTypeEntity(dto.getId());
        this.saveContactPersonType(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.CONTACT_PERSON_TYPE_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void deleteContactPersonType(String id) throws SystemException {
        if (!contactPersonTypeRepository.existsById(id)) {
            throw new SystemException(ErrorCode.CONTACT_PERSON_TYPE_NOT_EXISTS, id);
        }
        contactPersonTypeRepository.deleteById(id);
        appLogService.logDelete(id, DomainEnum.CONTACT_PERSON_TYPE_DOMAIN_NAME.getValue());
    }

    private void saveContactPersonType(ContactPersonTypeEntity entity, ContactPersonTypeDto dto) {
        ContactPersonTypeFactory.fillEntity(entity, dto);
        contactPersonTypeRepository.save(entity);
    }

    private ContactPersonTypeEntity getContactPersonTypeEntity(String id) throws SystemException {
        return contactPersonTypeRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.CONTACT_PERSON_TYPE_NOT_EXISTS, id));
    }
}
