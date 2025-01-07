package cz.bbn.cerberus.dssetting.persistance;

import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.dssetting.dto.DsSettingFilterDto;
import cz.bbn.cerberus.dssetting.dto.DsSettingSimpleDto;
import cz.bbn.cerberus.dssetting.factory.DsSettingFactory;
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
public class DsSettingDao {

    private final DsSettingSimpleRepository dsSettingSimpleRepository;

    public DsSettingDao(DsSettingSimpleRepository dsSettingSimpleRepository) {
        this.dsSettingSimpleRepository = dsSettingSimpleRepository;
    }

    public Page<DsSettingSimpleDto> findDsSettingDtoPage(DsSettingFilterDto filter) {
        Page<DsSettingSimpleEntity> page = dsSettingSimpleRepository.findAll(getDsSettingSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<DsSettingSimpleDto> list = ConvertEntities
                .fromEntities(page.toList(), DsSettingFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<DsSettingSimpleEntity> getDsSettingSpecification(DsSettingFilterDto filter) {
        return (Root<DsSettingSimpleEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getDsSettingPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getDsSettingPredicateList(DsSettingFilterDto filter, Root<DsSettingSimpleEntity> root,
                                                      CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        predicates.add(root.get("id").in(SecurityUtils.getCustomReadPermission(
                DomainEnum.DS_SETTING_DOMAIN_NAME.getValue())));

        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.isShowDeleted()));

        return predicates;
    }
}
