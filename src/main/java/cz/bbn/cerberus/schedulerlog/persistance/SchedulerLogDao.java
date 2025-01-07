package cz.bbn.cerberus.schedulerlog.persistance;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.schedulerlog.dto.SchedulerLogDto;
import cz.bbn.cerberus.schedulerlog.dto.SchedulerLogFilterDto;
import cz.bbn.cerberus.schedulerlog.factory.SchedulerLogFactory;
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
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

@Component
public class SchedulerLogDao {

    private final SchedulerLogRepository schedulerLogRepository;

    public SchedulerLogDao(SchedulerLogRepository schedulerLogRepository) {
        this.schedulerLogRepository = schedulerLogRepository;
    }

    public Page<SchedulerLogDto> findSchedulerLogPage(SchedulerLogFilterDto filter) {
        Page<SchedulerLogEntity> page = schedulerLogRepository.findAll(getSchedulerLogSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<SchedulerLogDto> list = ConvertEntities
                .fromEntities(page.toList(), SchedulerLogFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<SchedulerLogEntity> getSchedulerLogSpecification(SchedulerLogFilterDto filter) {
        return (Root<SchedulerLogEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getSchedulerFilterPage(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getSchedulerFilterPage(SchedulerLogFilterDto filter,
                                                   Root<SchedulerLogEntity> root, CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        if (!StringUtils.isEmpty(filter.getDescription())) {
            predicates.add(criteriaBuilder.equal(root.get("description"),
                    "%".concat(filter.getDescription().toLowerCase()).concat("%")));
        }

        if (filter.getDate() != null) {
            LocalDateTime from = filter.getDate().atStartOfDay();
            LocalDateTime to = filter.getDate().atTime(LocalTime.MAX).minusMinutes(1);
            predicates.add(criteriaBuilder.between(root.get("date"), from, to));
        }

        return predicates;
    }
}
