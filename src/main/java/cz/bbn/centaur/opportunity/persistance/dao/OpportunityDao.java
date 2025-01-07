package cz.bbn.cerberus.opportunity.persistance.dao;

import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.enums.PctValues;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.opportunity.dto.OpportunityDtoFilter;
import cz.bbn.cerberus.opportunity.dto.OpportunitySimpleDto;
import cz.bbn.cerberus.opportunity.factory.OpportunityFactory;
import cz.bbn.cerberus.opportunity.persistance.entity.OpportunitySimpleEntity;
import cz.bbn.cerberus.opportunity.persistance.repository.OpportunitySimpleRepository;
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
public class OpportunityDao {

    private final OpportunitySimpleRepository opportunitySimpleRepository;

    public OpportunityDao(OpportunitySimpleRepository opportunitySimpleRepository) {
        this.opportunitySimpleRepository = opportunitySimpleRepository;
    }

    public Page<OpportunitySimpleDto> getOpportunityPage(OpportunityDtoFilter opportunityDtoFilter) {
        Page<OpportunitySimpleEntity> page = opportunitySimpleRepository.findAll(
                getOpportunitySpecification(opportunityDtoFilter),
                PageRequest.of(opportunityDtoFilter.getPage(), opportunityDtoFilter.getSize(),
                        Sort.by(opportunityDtoFilter.getOrderList())));
        List<OpportunitySimpleDto> opportunityDtoList = ConvertEntities
                .fromEntities(page.toList(), OpportunityFactory::fromEntity);
        return new PageImpl<>(opportunityDtoList, PageRequest.of(opportunityDtoFilter.getPage(),
                opportunityDtoFilter.getSize(), Sort.by(opportunityDtoFilter.getOrderList())),
                page.getTotalElements());
    }


    private Specification<OpportunitySimpleEntity> getOpportunitySpecification(
            OpportunityDtoFilter opportunityDtoFilter) {
        return (Root<OpportunitySimpleEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(geOpportunityPredicateList(opportunityDtoFilter, root, criteriaBuilder)
                        .toArray(new Predicate[0]));
    }

    private List<Predicate> geOpportunityPredicateList(
            OpportunityDtoFilter filter, Root<OpportunitySimpleEntity> root, CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        if (filter.isOnlyEditPermission()) {
            predicates.add(root.get("id").in(
                    SecurityUtils.getAllowedEntityIdByDomain(Permission.OPPORTUNITY_EDIT.name(),
                        DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue())));
        } else if (!SecurityUtils.hasCustomReadAll(DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue())) {
            predicates.add(root.get("id").in(SecurityUtils.getCustomReadPermission(
                    DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue())));
        }

        if (!StringUtils.isEmpty(filter.getName())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("name")), "%".concat(filter.getName().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getSubjectId())) {
            predicates.add(criteriaBuilder.like(criteriaBuilder.lower(
                    root.get("subjectId")), "%".concat(filter.getSubjectId().toLowerCase()).concat("%")));
        }

        if (CollectionUtils.isNotEmpty(filter.getSubjectDtoSet())) {
            predicates.add(root.get("subjectId").in(filter.getSubjectDtoSet().stream()
                    .map(SubjectDto::getId)
                    .toList()));
        }

        if (CollectionUtils.isNotEmpty(filter.getStateSet())) {
            predicates.add(root.get("state").in(filter.getStateSet()));
        }

        if (CollectionUtils.isNotEmpty(filter.getProgressSet())) {
            predicates.add(root.get("progress").in(filter.getProgressSet().stream()
                    .map(PctValues::getPct)
                    .toList()));
        }

        if (CollectionUtils.isNotEmpty(filter.getSuccessChanceSet())) {
            predicates.add(root.get("successChance").in(filter.getSuccessChanceSet().stream()
                    .map(PctValues::getPct)
                    .toList()));
        }

        if (filter.getStartDateFrom() != null) {
            predicates.add(criteriaBuilder.greaterThanOrEqualTo(
                    root.get("startDate"), filter.getStartDateFrom()));
        }

        if (filter.getStartDateTo() != null) {
            predicates.add(criteriaBuilder.lessThanOrEqualTo(
                    root.get("startDate"), filter.getStartDateTo()));
        }
        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.isShowDeleted()));

        return predicates;
    }
}
