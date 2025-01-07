package cz.bbn.cerberus.technology.persistance;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.technology.dto.TechnologyDto;
import cz.bbn.cerberus.technology.dto.TechnologyFilterDto;
import cz.bbn.cerberus.technology.factory.TechnologyFactory;
import cz.bbn.cerberus.technology.persistance.entity.TechnologyEntity;
import cz.bbn.cerberus.technology.persistance.repository.TechnologyRepository;
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
public class TechnologyDao {

    private TechnologyRepository technologyRepository;

    public TechnologyDao(TechnologyRepository technologyRepository) {
        this.technologyRepository = technologyRepository;
    }

    public Page<TechnologyDto> findTechnologyPage(TechnologyFilterDto filter) {
        Page<TechnologyEntity> page = technologyRepository.findAll(getTechnologySpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<TechnologyDto> list = ConvertEntities
                .fromEntities(page.toList(), TechnologyFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<TechnologyEntity> getTechnologySpecification(TechnologyFilterDto filter) {
        return (Root<TechnologyEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getTechnologyPredicateList(filter, root, criteriaBuilder)
                        .toArray(new Predicate[0]));
    }

    private List<Predicate> getTechnologyPredicateList(TechnologyFilterDto filter, Root<TechnologyEntity> root,
                                                       CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.isShowDeleted()));

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
