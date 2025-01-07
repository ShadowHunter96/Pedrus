package cz.bbn.cerberus.email.persistence;

import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.email.dto.EmailFilterDto;
import cz.bbn.cerberus.email.dto.EmailSimpleDto;
import cz.bbn.cerberus.email.factory.EmailFactory;
import cz.bbn.cerberus.email.persistence.entity.EmailSimpleEntity;
import cz.bbn.cerberus.email.persistence.repository.EmailSimpleRepository;
import cz.bbn.cerberus.permission.Permission;
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
public class EmailDao {

    private final EmailSimpleRepository emailRepository;


    public EmailDao(EmailSimpleRepository emailRepository) {
        this.emailRepository = emailRepository;
    }

    public Page<EmailSimpleDto> findEmailDtoPage(EmailFilterDto filter) {
        Page<EmailSimpleEntity> page = emailRepository.findAll(getEmailSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<EmailSimpleDto> list = ConvertEntities
                .fromEntities(page.toList(), EmailFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<EmailSimpleEntity> getEmailSpecification(EmailFilterDto filter) {
        return (Root<EmailSimpleEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            query.distinct(true);
            return criteriaBuilder.and(getEmailPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
        };
    }

    private List<Predicate> getEmailPredicateList(EmailFilterDto filter, Root<EmailSimpleEntity> root,
                                                  CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        predicates.add(getAllowedCriteria(root, criteriaBuilder));

        if (filter.getSender() != null) {
            predicates.add(criteriaBuilder.like(root.get("sender"), "%".concat(filter.getSender()).concat("%")));
        }

        if (filter.getSubject() != null) {
            predicates.add(criteriaBuilder.like(root.get("subject"), "%".concat(filter.getSubject()).concat("%")));
        }

        if (filter.getFromDateTime() != null && filter.getToDateTime() != null) {
            predicates.add(criteriaBuilder.between(
                    root.get("dateAndTime"), filter.getFromDateTime(), filter.getToDateTime()));
        } else if (filter.getFromDateTime() != null) {
            predicates.add(criteriaBuilder.greaterThanOrEqualTo(root.get("dateAndTime"), filter.getFromDateTime()));
        } else if (filter.getToDateTime() != null) {
            predicates.add(criteriaBuilder.lessThanOrEqualTo(root.get("dateAndTime"), filter.getToDateTime()));
        }

        if (filter.getNoOfAttachments() != null) {
            predicates.add(criteriaBuilder.equal(root.get("noOfAttachments"), filter.getNoOfAttachments()));
        }

        if (filter.getCustomer() != null) {
            predicates.add(criteriaBuilder.equal(root.get("customer"), filter.getCustomer().getId()));
        }

        if (filter.getEntityType() != null) {
            predicates.add(criteriaBuilder.equal(root.get("entityType"), filter.getEntityType().getValue()));
        }

        if(filter.getEntityId() != null){
            predicates.add(criteriaBuilder.equal(root.get("entityId"), filter.getEntityId()));
        }

        if (filter.getItem() != null) {
            predicates.add(criteriaBuilder.equal(root.get("entityId"), filter.getItem().getId()));
        }

        if (filter.getSearchAll() != null) {
            predicates.add(criteriaBuilder.or(
                    criteriaBuilder.like(criteriaBuilder.lower(root.get("sender")),
                            "%".concat(filter.getSearchAll()).concat("%").toLowerCase()),
                    criteriaBuilder.like(criteriaBuilder.lower(root.get("subject")),
                            "%".concat(filter.getSearchAll()).concat("%").toLowerCase()),
                    criteriaBuilder.like(criteriaBuilder.lower(root.get("body")),
                            "%".concat(filter.getSearchAll()).concat("%").toLowerCase())
            ));
        }

        return predicates;
    }

    private Predicate getAllowedCriteria(Root<EmailSimpleEntity> root, CriteriaBuilder criteriaBuilder) {
        return criteriaBuilder.or(
                getDomainCriteria(root, criteriaBuilder, DomainEnum.OFFER_DOMAIN_NAME.getValue(),
                        Permission.OFFER_EMAIL_VIEW.name()),
                getDomainCriteria(root, criteriaBuilder, DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue(),
                        Permission.OPPORTUNITY_EMAIL_VIEW.name()),
                getDomainCriteria(root, criteriaBuilder, DomainEnum.CONTRACT_DOMAIN_NAME.getValue(),
                        Permission.CONTRACT_EMAIL_VIEW.name()),
                getDomainCriteria(root, criteriaBuilder, DomainEnum.PROJECT_DOMAIN_NAME.getValue(),
                        Permission.PROJECT_EMAIL_VIEW.name()),
                getDomainCriteria(root, criteriaBuilder, DomainEnum.SUBJECT_DOMAIN_NAME.getValue(),
                        Permission.SUBJECT_EMAIL_VIEW.name())
        );
    }

    private Predicate getDomainCriteria(Root<EmailSimpleEntity> root, CriteriaBuilder criteriaBuilder,
                                        String domainName, String permName) {
        if (SecurityUtils.hasCustomPermission(domainName,
                CustomPermissionService.ALL_PERMISSION, permName)) {
            return criteriaBuilder.equal(root.get("entityType"), domainName);
        }
        return criteriaBuilder.and(
                root.get("entityId").in(SecurityUtils.getAllowedEntityIdByDomain(permName, domainName)),
                criteriaBuilder.equal(root.get("entityType"), domainName));
    }
}
