package cz.bbn.cerberus.employee.persistance;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.employee.dto.EmployeeActive;
import cz.bbn.cerberus.employee.dto.EmployeeByObjectDto;
import cz.bbn.cerberus.employee.dto.EmployeeByObjectFilterDto;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.employee.dto.EmployeeFilterDto;
import cz.bbn.cerberus.employee.factory.EmployeeFactory;
import cz.bbn.cerberus.employee.persistance.entity.EmployeeByObjectEntity;
import cz.bbn.cerberus.employee.persistance.entity.EmployeeEntity;
import cz.bbn.cerberus.employee.persistance.repository.EmployeeByObjectRepository;
import cz.bbn.cerberus.employee.persistance.repository.EmployeeRepository;
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
public class EmployeeDao {

    private final EmployeeRepository employeeRepository;
    private final EmployeeByObjectRepository employeeByObjectRepository;

    public EmployeeDao(EmployeeRepository employeeRepository, EmployeeByObjectRepository employeeByObjectRepository) {
        this.employeeRepository = employeeRepository;
        this.employeeByObjectRepository = employeeByObjectRepository;
    }

    public Page<EmployeeDto> findEmployeePage(EmployeeFilterDto filter) {
        Page<EmployeeEntity> employeeEntityPage = employeeRepository.findAll(getEmployeeSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<EmployeeDto> employeeDtoList = ConvertEntities
                .fromEntities(employeeEntityPage.toList(), EmployeeFactory::fromEntity);
        return new PageImpl<>(employeeDtoList, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                employeeEntityPage.getTotalElements());
    }

    public Page<EmployeeByObjectDto> findEmployeeByObjectPage(EmployeeByObjectFilterDto filter) {
        Page<EmployeeByObjectEntity> employeeByObjectEntityPage = employeeByObjectRepository
                .findAll(getEmployeeByObjectSpecification(filter),
                        PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<EmployeeByObjectDto> employeeByObjectDtoList = ConvertEntities
                .fromEntities(employeeByObjectEntityPage.toList(), EmployeeFactory::fromEntity);
        return new PageImpl<>(employeeByObjectDtoList, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                employeeByObjectEntityPage.getTotalElements());
    }

    private Specification<EmployeeEntity> getEmployeeSpecification(EmployeeFilterDto filter) {
        return (Root<EmployeeEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getEmployeePredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private Specification<EmployeeByObjectEntity> getEmployeeByObjectSpecification(EmployeeByObjectFilterDto filter) {
        return (Root<EmployeeByObjectEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getEmployeeByObjectPredicateList(filter, root, criteriaBuilder)
                        .toArray(new Predicate[0]));
    }

    private List<Predicate> getEmployeeByObjectPredicateList(EmployeeByObjectFilterDto filter,
                                                             Root<EmployeeByObjectEntity> root,
                                                             CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(criteriaBuilder.notEqual(root.get("employeeEntity").get("deleted"), Boolean.TRUE));
        predicates.add(criteriaBuilder.equal(root.get("id").get("objectId"), filter.getObjectId()));
        predicates.add(criteriaBuilder.equal(root.get("id").get("objectType"), filter.getObjectType()));

        if (filter.getEmployeeActive() != EmployeeActive.ALL) {
            if (filter.getEmployeeActive() == EmployeeActive.ACTIVE) {
                predicates.add(criteriaBuilder.equal(root.get("employeeEntity").get("active"), Boolean.TRUE));
            } else {
                predicates.add(criteriaBuilder.equal(root.get("employeeEntity").get("active"), Boolean.FALSE));
            }
        }
        return predicates;
    }

    private List<Predicate> getEmployeePredicateList(EmployeeFilterDto filter, Root<EmployeeEntity> root,
                                                     CriteriaBuilder criteriaBuilder) {

        List<Predicate> predicates = new ArrayList<>();

        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.isShowDeleted()));

        if (!StringUtils.isEmpty(filter.getId())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("id")), "%".concat(filter.getId().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getFirstName())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("firstName")), "%".concat(filter.getFirstName()
                            .toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getLastName())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("lastName")), "%".concat(filter.getLastName()
                            .toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getCompanyEmail())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("companyEmail")), "%".concat(filter.getCompanyEmail()
                            .toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getCompanyPhoneNumber())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("companyPhoneNumber")), "%".concat(filter.getCompanyPhoneNumber()
                            .toLowerCase()).concat("%")));
        }

        return predicates;
    }
}
