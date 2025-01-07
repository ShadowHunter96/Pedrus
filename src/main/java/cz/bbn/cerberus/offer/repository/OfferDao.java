package cz.bbn.cerberus.offer.repository;

import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.offer.dto.OfferDto;
import cz.bbn.cerberus.offer.dto.OfferFilterDto;
import cz.bbn.cerberus.offer.factory.OfferFactory;
import cz.bbn.cerberus.offer.repository.entity.OfferEntity;
import cz.bbn.cerberus.offer.repository.repository.OfferRepository;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import org.apache.commons.collections4.CollectionUtils;
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
import java.util.List;

@Component
public class OfferDao {

    private final OfferRepository offerRepository;

    public OfferDao(OfferRepository offerRepository) {
        this.offerRepository = offerRepository;
    }

    public Page<OfferDto> findOfferPage(OfferFilterDto filter) {
        Page<OfferEntity> page = offerRepository.findAll(getOfferSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<OfferDto> list = ConvertEntities
                .fromEntities(page.toList(), OfferFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<OfferEntity> getOfferSpecification(OfferFilterDto filter) {
        return (Root<OfferEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getOfferPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getOfferPredicateList(OfferFilterDto filter, Root<OfferEntity> root,
                                                  CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        if (!SecurityUtils.hasPermission(Permission.OFFER_LIST_VIEW)
                && !SecurityUtils.hasCustomReadAll(DomainEnum.OFFER_DOMAIN_NAME.getValue())) {
            predicates.add(root.get("id").in(
                    SecurityUtils.getCustomReadPermission(DomainEnum.OFFER_DOMAIN_NAME.getValue())));
        }

        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.isShowDeleted()));

        if (filter.getObjectType() == ObjectType.OPPORTUNITY) {
            predicates.add(criteriaBuilder.equal(root.get("opportunityEntity").get("id"), filter.getObjectId()));
        }

        if (filter.getObjectType() == ObjectType.SUBJECT) {
            predicates.add(criteriaBuilder.equal(
                    root.get("opportunityEntity").get("subject").get("id"), filter.getObjectId()));
        }

        if (filter.isOnlyEditPermission()) {
            predicates.add(root.get("id").in(
                    SecurityUtils.getAllowedEntityIdByDomain(Permission.OFFER_EDIT.name(),
                            DomainEnum.OFFER_DOMAIN_NAME.getValue())));
        }

        if (CollectionUtils.isNotEmpty(filter.getCustomerSet())) {
            predicates.add(root.get("subjectEntity").get("id").in(filter.getCustomerSet().stream()
                    .map(SubjectDto::getId)
                    .toList()));
        }

        if (filter.getPriceFrom() != null) {
            predicates.add(criteriaBuilder.greaterThanOrEqualTo(root.get("priceWithoutVat"), filter.getPriceFrom()));
        }

        if (filter.getPriceTo() != null) {
            predicates.add(criteriaBuilder.lessThanOrEqualTo(root.get("priceWithoutVat"), filter.getPriceTo()));
        }

        if (!StringUtils.isEmpty(filter.getNameOrId())) {
            predicates.add(criteriaBuilder.or(criteriaBuilder.like(criteriaBuilder.lower(root.get("name")),
                            "%".concat(filter.getNameOrId().toLowerCase()).concat("%")),
                    criteriaBuilder.like(criteriaBuilder.lower(root.get("id")),
                            "%".concat(filter.getNameOrId().toLowerCase()).concat("%"))));
        }

        if (CollectionUtils.isNotEmpty(filter.getSent())) {
            predicates.add(root.get("sent").in(filter.getSent()));
        }

        if (CollectionUtils.isNotEmpty(filter.getOfferStateSet())) {
            predicates.add(root.get("state").in(filter.getOfferStateSet()));
        }

        if (filter.getValidityDateFrom() != null) {
            predicates.add(
                    criteriaBuilder.greaterThanOrEqualTo(root.get("validityDate"), filter.getValidityDateFrom()));
        }

        if (filter.getValidityDateTo() != null) {
            predicates.add(criteriaBuilder.lessThanOrEqualTo(root.get("validityDate"), filter.getValidityDateTo()));
        }

        return predicates;
    }
}
