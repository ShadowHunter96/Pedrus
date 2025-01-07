package cz.bbn.cerberus.phase.repository;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.phase.dto.PhaseDto;
import cz.bbn.cerberus.phase.dto.PhaseFilterDto;
import cz.bbn.cerberus.phase.factory.PhaseFactory;
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
public class PhaseDao {

    private final PhaseRepository phaseRepository;

    public PhaseDao(PhaseRepository phaseRepository) {
        this.phaseRepository = phaseRepository;
    }

    public Page<PhaseDto> findPhasePage(PhaseFilterDto filter) {
        Page<PhaseEntity> page = phaseRepository.findAll(getPhaseByObjectSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<PhaseDto> list = ConvertEntities
                .fromEntities(page.toList(), PhaseFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<PhaseEntity> getPhaseByObjectSpecification(PhaseFilterDto filter) {
        return (Root<PhaseEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(
                        getPhaseByObjectPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getPhaseByObjectPredicateList(PhaseFilterDto filter, Root<PhaseEntity> root,
                                                          CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(criteriaBuilder.equal(root.get("projectId"), filter.getProjectId()));
        return predicates;
    }
}
