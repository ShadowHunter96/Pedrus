package cz.bbn.cerberus.applog.persistance;

import cz.bbn.cerberus.applog.dto.AppLogDto;
import cz.bbn.cerberus.applog.dto.AppLogFilterDto;
import cz.bbn.cerberus.applog.factory.AppLogFactory;
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
public class AppLogDao {

    private final AppLogRepository appLogRepository;

    public AppLogDao(AppLogRepository appLogRepository) {
        this.appLogRepository = appLogRepository;
    }

    public Page<AppLogDto> findAppLogDtoPage(AppLogFilterDto filter) {
        Page<AppLogEntity> page = appLogRepository.findAll(getAppLogSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<AppLogDto> list = ConvertEntities
                .fromEntities(page.toList(), AppLogFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<AppLogEntity> getAppLogSpecification(AppLogFilterDto filter) {
        return (Root<AppLogEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getAppLogList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getAppLogList(AppLogFilterDto filter,
                                          Root<AppLogEntity> root,
                                          CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        if (!StringUtils.isEmpty(filter.getAction())) {
            predicates.add(criteriaBuilder.equal(root.get("action"), filter.getAction()));
        }

        if (filter.getUserId() != null && filter.getUserId() != 0) {
            predicates.add(criteriaBuilder.equal(root.get("userId"), filter.getUserId()));
        }

        if (!StringUtils.isEmpty(filter.getMessage())) {
            predicates.add(criteriaBuilder.like(root.get("message"), "%".concat(filter.getMessage()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getAppId())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("appId")), filter.getAppId().toLowerCase()));
        }

        return predicates;
    }
}
