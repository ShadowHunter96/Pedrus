package cz.bbn.cerberus.enumeration.persistance;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationFilterDto;
import cz.bbn.cerberus.enumeration.factory.EnumerationFactory;
import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationEntity;
import cz.bbn.cerberus.enumeration.persistance.repository.EnumerationRepository;
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
public class EnumerationDao {

    private final EnumerationRepository enumerationRepository;

    public EnumerationDao(EnumerationRepository enumerationRepository) {
        this.enumerationRepository = enumerationRepository;
    }

    public Page<EnumerationDto> findEnumerationPage(EnumerationFilterDto filter) {
        Page<EnumerationEntity> page = enumerationRepository.findAll(getEnumerationSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<EnumerationDto> list = ConvertEntities
                .fromEntities(page.toList(), EnumerationFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<EnumerationEntity> getEnumerationSpecification(EnumerationFilterDto filter) {
        return (Root<EnumerationEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(
                        getEnumerationPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getEnumerationPredicateList(EnumerationFilterDto filter, Root<EnumerationEntity> root,
                                                        CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        predicates.add(criteriaBuilder.equal(root.get("allowed"), !filter.isShowNotAllowed()));
        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.isShowDeleted()));

        predicates.add(criteriaBuilder.equal(
                root.get("enumerationTypeEntity").get("id"), filter.getEnumerationTypeId()));

        if (!StringUtils.isEmpty(filter.getId())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("id")), "%".concat(filter.getId().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getName())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("name")), "%".concat(filter.getName().toLowerCase()).concat("%")));
        }

        return predicates;
    }
}
