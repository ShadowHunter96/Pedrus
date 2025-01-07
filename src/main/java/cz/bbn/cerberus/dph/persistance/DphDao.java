package cz.bbn.cerberus.dph.persistance;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.dph.dto.DphDto;
import cz.bbn.cerberus.dph.dto.DphFilterDto;
import cz.bbn.cerberus.dph.factory.DphFactory;
import cz.bbn.cerberus.dph.persistance.entity.DphEntity;
import cz.bbn.cerberus.dph.persistance.repository.DphRepository;
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
public class DphDao {

    private final DphRepository dphRepository;

    public DphDao(DphRepository dphRepository) {
        this.dphRepository = dphRepository;
    }

    public Page<DphDto> findDphPage(DphFilterDto filter) {
        Page<DphEntity> page = dphRepository.findAll(getDphSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<DphDto> list = ConvertEntities
                .fromEntities(page.toList(), DphFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<DphEntity> getDphSpecification(DphFilterDto filter) {
        return (Root<DphEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getDphPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getDphPredicateList(DphFilterDto filter, Root<DphEntity> root,
                                                CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        predicates.add(criteriaBuilder.equal(root.get("allowed"), !filter.isShowNotAllowed()));

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
