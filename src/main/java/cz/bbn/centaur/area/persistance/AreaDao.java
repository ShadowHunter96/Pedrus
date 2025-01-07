package cz.bbn.cerberus.area.persistance;

import cz.bbn.cerberus.area.dto.AreaDto;
import cz.bbn.cerberus.area.dto.AreaFilterDto;
import cz.bbn.cerberus.area.factory.AreaFactory;
import cz.bbn.cerberus.area.persistance.entity.AreaEntity;
import cz.bbn.cerberus.area.persistance.repository.AreaRepository;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
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
public class AreaDao {

    private AreaRepository areaRepository;

    public AreaDao(AreaRepository areaRepository) {
        this.areaRepository = areaRepository;
    }

    public Page<AreaDto> findAreaPage(AreaFilterDto filter) {
        Page<AreaEntity> page = areaRepository.findAll(getAreaSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<AreaDto> list = ConvertEntities
                .fromEntities(page.toList(), AreaFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<AreaEntity> getAreaSpecification(AreaFilterDto filter) {
        return (Root<AreaEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getAreaPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getAreaPredicateList(AreaFilterDto filter, Root<AreaEntity> root,
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
