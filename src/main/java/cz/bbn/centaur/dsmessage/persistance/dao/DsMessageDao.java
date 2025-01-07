package cz.bbn.cerberus.dsmessage.persistance.dao;

import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.dsmessage.dto.DsMessageFilterDto;
import cz.bbn.cerberus.dsmessage.dto.DsMessageSimpleDto;
import cz.bbn.cerberus.dsmessage.factory.DsMessageFactory;
import cz.bbn.cerberus.dsmessage.persistance.DsMessageType;
import cz.bbn.cerberus.dsmessage.persistance.entity.DsMessageSimpleEntity;
import cz.bbn.cerberus.dsmessage.persistance.repository.DsMessageSimpleRepository;
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
public class DsMessageDao {

    private final DsMessageSimpleRepository dsMessageSimpleRepository;

    public DsMessageDao(DsMessageSimpleRepository dsMessageSimpleRepository) {
        this.dsMessageSimpleRepository = dsMessageSimpleRepository;
    }

    public Page<DsMessageSimpleDto> findDsMessageDtoPage(DsMessageFilterDto filter) {
        Page<DsMessageSimpleEntity> page = dsMessageSimpleRepository.findAll(getDsMessageSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<DsMessageSimpleDto> list = ConvertEntities
                .fromEntities(page.toList(), DsMessageFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<DsMessageSimpleEntity> getDsMessageSpecification(DsMessageFilterDto filter) {
        return (Root<DsMessageSimpleEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getDsMessagePredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getDsMessagePredicateList(DsMessageFilterDto filter, Root<DsMessageSimpleEntity> root,
                                                      CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        if (!SecurityUtils.hasCustomReadAll(DomainEnum.DS_MESSAGE_DOMAIN_NAME.getValue())) {
            predicates.add(root.get("recipientId").in(SecurityUtils.getCustomReadPermission(
                    DomainEnum.DS_MESSAGE_DOMAIN_NAME.getValue())));
        }

        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.isShowDeleted()));

        if (filter.getDeliveryTimeFrom() != null && filter.getDeliveryTimeTo() != null) {
            predicates.add(criteriaBuilder.between(
                    root.get("deliveryTime"), filter.getDeliveryTimeFrom(), filter.getDeliveryTimeTo()));
        } else if (filter.getDeliveryTimeFrom() != null) {
            predicates.add(criteriaBuilder.greaterThanOrEqualTo(
                    root.get("deliveryTime"), filter.getDeliveryTimeFrom()));
        } else if (filter.getDeliveryTimeTo() != null) {
            predicates.add(criteriaBuilder.lessThanOrEqualTo(root.get("deliveryTime"), filter.getDeliveryTimeTo()));
        }

        if (filter.getCreatedInAppFrom() != null && filter.getCreatedInAppTo() != null) {
            predicates.add(criteriaBuilder.between(
                    root.get("createdInApp"), filter.getCreatedInAppFrom(), filter.getCreatedInAppTo()));
        } else if (filter.getCreatedInAppFrom() != null) {
            predicates.add(criteriaBuilder.greaterThanOrEqualTo(
                    root.get("createdInApp"), filter.getCreatedInAppFrom()));
        } else if (filter.getCreatedInAppTo() != null) {
            predicates.add(criteriaBuilder.lessThanOrEqualTo(root.get("createdInApp"), filter.getCreatedInAppTo()));
        }

        if (StringUtils.isNoneEmpty(filter.getRecipientId())) {
            predicates.add(criteriaBuilder.equal(root.get("recipientId"), filter.getRecipientId()));
        }

        if (StringUtils.isNoneEmpty(filter.getSenderName())) {
            predicates.add(criteriaBuilder.equal(root.get("senderName"), filter.getSenderName()));
        }

        if (filter.isViewed()) {
            predicates.add(criteriaBuilder.equal(root.get("viewed"), filter.isViewed()));
        }

        if (filter.getType() != DsMessageType.ALL) {
            predicates.add(criteriaBuilder.equal(root.get("type"), filter.getType()));
        }

        return predicates;
    }
}
