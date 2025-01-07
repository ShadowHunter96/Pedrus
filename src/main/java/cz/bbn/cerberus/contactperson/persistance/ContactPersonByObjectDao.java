package cz.bbn.cerberus.contactperson.persistance;

import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactperson.dto.ContactPersonByObjectDto;
import cz.bbn.cerberus.contactperson.dto.ContactPersonByObjectFilterDto;
import cz.bbn.cerberus.contactperson.factory.ContactPersonByObjectFactory;
import cz.bbn.cerberus.contactperson.persistance.entity.ContactPersonByObjectEntity;
import cz.bbn.cerberus.contactperson.persistance.repository.ContactPersonByObjectRepository;
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
import java.util.List;

@Component
public class ContactPersonByObjectDao {

    private final ContactPersonByObjectRepository repository;

    public ContactPersonByObjectDao(ContactPersonByObjectRepository repository) {
        this.repository = repository;
    }

    public Page<ContactPersonByObjectDto> findContactPersonByObjectPage(ContactPersonByObjectFilterDto filter) {
        Page<ContactPersonByObjectEntity> page = repository.findAll(getContactPersonByObjectSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<ContactPersonByObjectDto> list = ConvertEntities
                .fromEntities(page.toList(), ContactPersonByObjectFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<ContactPersonByObjectEntity> getContactPersonByObjectSpecification(
            ContactPersonByObjectFilterDto filter
    ) {
        return (Root<ContactPersonByObjectEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(
                        getContactPersonByObjectPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getContactPersonByObjectPredicateList(
            ContactPersonByObjectFilterDto filter,
            Root<ContactPersonByObjectEntity> root, CriteriaBuilder criteriaBuilder
    ) {
        List<Predicate> predicates = new ArrayList<>();

        if (!SecurityUtils.hasCustomReadAll(DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue())) {
            predicates.add(root.get("contactPerson").get("id").in(SecurityUtils.getCustomReadPermission(
                    DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue())));
        }

        predicates.add(criteriaBuilder.equal(root.join("contactPerson").get("deleted"), filter.isShowDeleted()));

        predicates.add(criteriaBuilder.equal(root.get("objectType"), filter.getObjectType().toString()));

        predicates.add(criteriaBuilder.equal(root.get("addedToObjectId"), filter.getObjectId()));

        return predicates;
    }

}
