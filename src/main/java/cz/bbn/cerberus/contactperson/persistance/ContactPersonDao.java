package cz.bbn.cerberus.contactperson.persistance;

import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactperson.dto.ContactPersonDto;
import cz.bbn.cerberus.contactperson.dto.ContactPersonFilterDto;
import cz.bbn.cerberus.contactperson.dto.ContactPersonObjectTypeEnum;
import cz.bbn.cerberus.contactperson.factory.ContactPersonFactory;
import cz.bbn.cerberus.contactperson.persistance.entity.ContactPersonEntity;
import cz.bbn.cerberus.contactperson.persistance.repository.ContactPersonByObjectRepository;
import cz.bbn.cerberus.contactperson.persistance.repository.ContactPersonRepository;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Component
public class ContactPersonDao {

    private final ContactPersonRepository contactPersonRepository;
    private final ContactPersonByObjectRepository contactPersonByObjectRepository;

    public ContactPersonDao(ContactPersonRepository contactPersonRepository,
                            ContactPersonByObjectRepository contactPersonByObjectRepository) {
        this.contactPersonRepository = contactPersonRepository;
        this.contactPersonByObjectRepository = contactPersonByObjectRepository;
    }

    public Page<ContactPersonDto> findContactPersonPage(ContactPersonFilterDto filter) {
        return findPage(filter, new HashSet<>());
    }

    public Page<ContactPersonDto> findContactPersonXorByIdList(ContactPersonFilterDto filter,
                                                               Set<String> excludedIdSet) {
        return findPage(filter, excludedIdSet);
    }

    private Page<ContactPersonDto> findPage(ContactPersonFilterDto filter, Set<String> excludedSet) {
        Page<ContactPersonEntity> page =
                contactPersonRepository.findAll(getContactPersonSpecification(filter, excludedSet),
                        PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<ContactPersonDto> list = ConvertEntities
                .fromEntities(page.toList(), ContactPersonFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<ContactPersonEntity> getContactPersonSpecification(ContactPersonFilterDto filter,
                                                                             Set<String> excludeIdSet) {
        return (Root<ContactPersonEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getContactPersonPredicateList(
                        filter, root, criteriaBuilder, excludeIdSet
                ).toArray(new Predicate[0]));
    }

    private List<Predicate> getContactPersonPredicateList(
            ContactPersonFilterDto filter,
            Root<ContactPersonEntity> root,
            CriteriaBuilder criteriaBuilder,
            Set<String> excludeIdSet
    ) {
        List<Predicate> predicates = new ArrayList<>();

        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.isShowDeleted()));

        if (!SecurityUtils.hasCustomReadAll(DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue())) {
            predicates.add(root.get("id").in(SecurityUtils.getCustomReadPermission(
                    DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue())));
        }

        if (!excludeIdSet.isEmpty()) {
            Predicate in = root.get("id").in(excludeIdSet);
            predicates.add(criteriaBuilder.not(in));
        }

        if (!StringUtils.isEmpty(filter.getId())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("id")), "%".concat(filter.getId().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getFirstName())) {
            predicates.add(criteriaBuilder.like(criteriaBuilder.lower(root.get("firstName")),
                    "%".concat(filter.getFirstName().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getLastName())) {
            predicates.add(criteriaBuilder.like(criteriaBuilder.lower(root.get("lastName")),
                    "%".concat(filter.getLastName().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getEmail())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("email")), "%".concat(filter.getEmail().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getPhone())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("phone")), "%".concat(filter.getPhone().toLowerCase()).concat("%")));
        }

        if (filter.getContactPersonType() != null) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("contactPersonTypeEntity").get("id")),
                    filter.getContactPersonType().toLowerCase()));
        }

        if (filter.getSubjectId() != null) {
            predicates.add(root.get("id").in(getContactPersonIdSetByObjectTypeAndObjectId(
                    ContactPersonObjectTypeEnum.SUBJECT, filter.getSubjectId())));
        }

        if (filter.getProjectId() != null) {
            predicates.add(root.get("id").in(getContactPersonIdSetByObjectTypeAndObjectId(
                    ContactPersonObjectTypeEnum.PROJECT, filter.getProjectId())));
        }

        return predicates;
    }

    public Set<String> getAllowedContactPerson() {
        return SecurityUtils.getCustomReadPermission(DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue());
    }

    private Set<String> getContactPersonIdSetByObjectTypeAndObjectId(
            ContactPersonObjectTypeEnum objectType, String objectId
    ) {
        Set<String> objectIdSet = new HashSet<>();
        objectIdSet.add(objectId);
        return contactPersonByObjectRepository.findIdByObjectTypeAndObjectIdSet(objectType.toString(), objectIdSet);
    }
}
