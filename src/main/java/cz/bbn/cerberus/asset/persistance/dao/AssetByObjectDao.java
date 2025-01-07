package cz.bbn.cerberus.asset.persistance.dao;

import cz.bbn.cerberus.asset.dto.AssetByObjectDto;
import cz.bbn.cerberus.asset.dto.AssetByObjectFilterDto;
import cz.bbn.cerberus.asset.factory.AssetByObjectFactory;
import cz.bbn.cerberus.asset.persistance.entity.AssetByObjectEntity;
import cz.bbn.cerberus.asset.persistance.repository.AssetByObjectRepository;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
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
public class AssetByObjectDao {

    private final AssetByObjectRepository assetByObjectRepository;

    public AssetByObjectDao(AssetByObjectRepository assetByObjectRepository) {
        this.assetByObjectRepository = assetByObjectRepository;
    }

    public Page<AssetByObjectDto> findAssetByObjectPage(AssetByObjectFilterDto filter) {
        Page<AssetByObjectEntity> page = assetByObjectRepository.findAll(getAssetSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<AssetByObjectDto> list = ConvertEntities
                .fromEntities(page.toList(), AssetByObjectFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<AssetByObjectEntity> getAssetSpecification(AssetByObjectFilterDto filter) {
        return (Root<AssetByObjectEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getAssetPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getAssetPredicateList(AssetByObjectFilterDto filter, Root<AssetByObjectEntity> root,
                                                  CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        predicates.add(criteriaBuilder.equal(root.get("asset").get("deleted"), false));

        predicates.add(criteriaBuilder.equal(root.get("objectType"), filter.getObjectType().name()));

        predicates.add(criteriaBuilder.equal(root.get("addedToObjectId"), filter.getObjectId()));

        return predicates;
    }
}
