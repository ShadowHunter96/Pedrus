package cz.bbn.cerberus.asset.persistance.dao;

import cz.bbn.cerberus.asset.dto.AssetFilterDto;
import cz.bbn.cerberus.asset.dto.AssetSimpleDto;
import cz.bbn.cerberus.asset.factory.AssetFactory;
import cz.bbn.cerberus.asset.persistance.entity.AssetSimpleEntity;
import cz.bbn.cerberus.asset.persistance.repository.AssetSimpleRepository;
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
import java.util.Set;

@Component
public class AssetDao {

    private final AssetSimpleRepository assetSimpleRepository;

    public AssetDao(AssetSimpleRepository assetSimpleRepository) {
        this.assetSimpleRepository = assetSimpleRepository;
    }

    public Page<AssetSimpleDto> findAssetPage(AssetFilterDto filter) {
        Page<AssetSimpleEntity> page = assetSimpleRepository.findAll(getAssetSpecification(filter, null, null),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<AssetSimpleDto> list = ConvertEntities
                .fromEntities(page.toList(), AssetFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    public Page<AssetSimpleDto> findAssetPage(AssetFilterDto filter, String id, Set<String> existingConnectionIdSet) {
        Page<AssetSimpleEntity> page = assetSimpleRepository.findAll(
                getAssetSpecification(filter, id, existingConnectionIdSet),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<AssetSimpleDto> list = ConvertEntities
                .fromEntities(page.toList(), AssetFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<AssetSimpleEntity> getAssetSpecification(
            AssetFilterDto filter, String id, Set<String> existingConnectionIdSet) {
        return (Root<AssetSimpleEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getAssetPredicateList(filter, root, criteriaBuilder, id, existingConnectionIdSet)
                        .toArray(new Predicate[0]));
    }

    private List<Predicate> getAssetPredicateList(AssetFilterDto filter, Root<AssetSimpleEntity> root,
                                                  CriteriaBuilder criteriaBuilder, String id,
                                                  Set<String> existingConnectionIdSet) {

        List<Predicate> predicates = new ArrayList<>();

        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.isShowDeleted()));

        if (id != null) {
            predicates.add(criteriaBuilder.not(criteriaBuilder.equal(root.get("id"), id)));
        }

        if (existingConnectionIdSet != null && !existingConnectionIdSet.isEmpty()) {
            predicates.add(criteriaBuilder.not(root.get("id").in(existingConnectionIdSet)));
        }

        if (!StringUtils.isEmpty(filter.getId())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("id")), "%".concat(filter.getId().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getName())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("name")), "%".concat(filter.getName().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getSerialNumber())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("serialNumber")),
                    "%".concat(filter.getSerialNumber().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getOwner())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("owner")), "%".concat(filter.getOwner().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getType())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("type")), "%".concat(filter.getType().toLowerCase()).concat("%")));
        }
        return predicates;
    }
}
