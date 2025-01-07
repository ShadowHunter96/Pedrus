package cz.bbn.cerberus.approvement.persistance.dao;

import cz.bbn.cerberus.approvement.dto.ApprovementFilterDto;
import cz.bbn.cerberus.approvement.dto.ApprovementSimpleDto;
import cz.bbn.cerberus.approvement.factory.ApprovementFactory;
import cz.bbn.cerberus.approvement.persistance.entity.ApprovementEntity;
import cz.bbn.cerberus.approvement.persistance.repository.ApprovementRepository;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
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
public class ApprovementDao {

    private final ApprovementRepository approvementRepository;

    public ApprovementDao(ApprovementRepository approvementRepository) {
        this.approvementRepository = approvementRepository;
    }

    public Page<ApprovementSimpleDto> findApprovementDtoPage(ApprovementFilterDto filter) {
        Page<ApprovementEntity> page = approvementRepository.findAll(getApprovementSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<ApprovementSimpleDto> list = ConvertEntities
                .fromEntities(page.toList(), ApprovementFactory::fromSimpleEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<ApprovementEntity> getApprovementSpecification(ApprovementFilterDto filter) {
        return (Root<ApprovementEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getApprovementList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getApprovementList(ApprovementFilterDto filter,
                                               Root<ApprovementEntity> root,
                                               CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        Predicate firstDatePredicate =
                criteriaBuilder.and(criteriaBuilder.equal(
                                criteriaBuilder.function("YEAR", Integer.class, root.get("dateFrom")), filter.getYear()),
                        criteriaBuilder.equal(
                                criteriaBuilder.function("YEAR", Integer.class, root.get("dateTo")), filter.getYear() + 1));

        Predicate secondDatePredicate =
                criteriaBuilder.and(criteriaBuilder.equal(
                                criteriaBuilder.function("YEAR", Integer.class, root.get("dateFrom")), filter.getYear() - 1),
                        criteriaBuilder.equal(
                                criteriaBuilder.function("YEAR", Integer.class, root.get("dateTo")), filter.getYear()));

        Predicate thirdDatePredicate = criteriaBuilder.and(criteriaBuilder.equal(
                        criteriaBuilder.function("YEAR", Integer.class, root.get("dateFrom")), filter.getYear()),
                criteriaBuilder.equal(
                        criteriaBuilder.function("YEAR", Integer.class, root.get("dateTo")), filter.getYear()));

        predicates.add(criteriaBuilder.or(firstDatePredicate, secondDatePredicate, thirdDatePredicate));

        if (filter.getId() != null) {
            predicates.add(criteriaBuilder.equal(root.get("id"), filter.getId()));
        }

        if (!filter.isMaster() && filter.isApprovement()) {
            predicates.add(criteriaBuilder.or(criteriaBuilder.equal(root.get("lineManagerUserEntity").get("id"), SecurityUtils.getCurrentUserId()),
                    criteriaBuilder.equal(root.get("superiorUserEntity").get("id"), SecurityUtils.getCurrentUserId()),
                    root.get("lineManagerRoleEntity").get("id").in(SecurityUtils.getCurrentUser().getActiveRoleSet())));
        }


        if (filter.getDateFrom() != null) {
            predicates.add(criteriaBuilder.greaterThanOrEqualTo(root.get("dateFrom"), filter.getDateFrom()));
        }
        if (filter.getDateTo() != null) {
            predicates.add(criteriaBuilder.lessThanOrEqualTo(root.get("dateFrom"), filter.getDateTo()));
        }
        if (!filter.getEmployeeDtoSet().isEmpty()) {
            predicates.add(root.get("ownerUserEntity").get("employee").get("id").in(filter.getEmployeeDtoSet().stream()
                    .map(EmployeeDto::getId)
                    .toList()));
        }
        if (!filter.getStateSet().isEmpty()) {
            predicates.add(root.get("approvementState").in(filter.getStateSet().stream()
                    .toList()));
        }
        if (!filter.getTypeSet().isEmpty()) {
            predicates.add(root.get("approvementType").in(filter.getTypeSet().stream()
                    .toList()));
        }
        return predicates;
    }
}
