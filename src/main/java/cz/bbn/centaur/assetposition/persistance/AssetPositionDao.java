package cz.bbn.cerberus.assetposition.persistance;

import cz.bbn.cerberus.assetposition.dto.AssetPositionDto;
import cz.bbn.cerberus.assetposition.dto.AssetPositionFilterDto;
import cz.bbn.cerberus.assetposition.factory.AssetPositionFactory;
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
public class AssetPositionDao {

    private final AssetPositionRepository assetPositionRepository;

    public AssetPositionDao(AssetPositionRepository assetPositionRepository) {
        this.assetPositionRepository = assetPositionRepository;
    }

    public Page<AssetPositionDto> findAssetPositionPage(AssetPositionFilterDto filter) {
        Page<AssetPositionEntity> page = assetPositionRepository.findAll(getAssetSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<AssetPositionDto> list = ConvertEntities
                .fromEntities(page.toList(), AssetPositionFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<AssetPositionEntity> getAssetSpecification(AssetPositionFilterDto filter) {
        return (Root<AssetPositionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getAssetPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getAssetPredicateList(AssetPositionFilterDto filter, Root<AssetPositionEntity> root,
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
