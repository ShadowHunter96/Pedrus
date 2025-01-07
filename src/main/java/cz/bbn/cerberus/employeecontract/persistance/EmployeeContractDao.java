package cz.bbn.cerberus.employeecontract.persistance;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.employeecontract.dto.EmployeeContractDto;
import cz.bbn.cerberus.employeecontract.dto.EmployeeContractFilterDto;
import cz.bbn.cerberus.employeecontract.factory.EmployeeContractFactory;
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
public class EmployeeContractDao {

    private final EmployeeContractRepository employeeContractRepository;

    public EmployeeContractDao(EmployeeContractRepository employeeContractRepository) {
        this.employeeContractRepository = employeeContractRepository;
    }

    public List<EmployeeContractDto> findAllAllowedEmployeeContractList() {
        EmployeeContractFilterDto filterDto = new EmployeeContractFilterDto();
        filterDto.setShowDeleted(false);
        filterDto.setShowArchived(false);
        List<EmployeeContractEntity> employeeContractEntityList =
                employeeContractRepository.findAll(getEmployeeContractSpecification(filterDto));
        return ConvertEntities.fromEntities(employeeContractEntityList, EmployeeContractFactory::fromEntity);
    }

    public Page<EmployeeContractDto> findEmployeeContractPage(EmployeeContractFilterDto filter) {
        Page<EmployeeContractEntity> employeeContractEntityPage =
                employeeContractRepository.findAll(getEmployeeContractSpecification(filter),
                        PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<EmployeeContractDto> employeeContractDtoList = ConvertEntities
                .fromEntities(employeeContractEntityPage.toList(), EmployeeContractFactory::fromEntity);
        return new PageImpl<>(employeeContractDtoList, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                employeeContractEntityPage.getTotalElements());
    }

    private Specification<EmployeeContractEntity> getEmployeeContractSpecification(EmployeeContractFilterDto filter) {
        return (Root<EmployeeContractEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getEmployeeContractPredicateList(filter, root, criteriaBuilder)
                        .toArray(new Predicate[0]));
    }

    private List<Predicate> getEmployeeContractPredicateList(
            EmployeeContractFilterDto filter, Root<EmployeeContractEntity> root, CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        if (filter.getId() != null && !filter.getId().isEmpty()) {
            predicates.add(criteriaBuilder.like(root.get("id"), "%".concat(filter.getId()).concat("%")));
        }

        if (filter.getEmployee() != null) {
            predicates.add(criteriaBuilder.equal(root.get("employee").get("id"), filter.getEmployee().getId()));
        }

        if (filter.getState() != null) {
            predicates.add(criteriaBuilder.equal(root.get("state").get("id"), filter.getState().getId()));
        }

        if (filter.getType() != null) {
            predicates.add(criteriaBuilder.equal(root.get("type").get("id"), filter.getType().getId()));
        }

        if (filter.getCompany() != null) {
            predicates.add(criteriaBuilder.equal(root.get("ownCompany").get("id"), filter.getCompany().getId()));
        }

        if (filter.getValidTo() != null) {
            predicates.add(criteriaBuilder.greaterThanOrEqualTo(root.get("validTo"), filter.getValidTo()));
        }

        if (filter.getLinkedContract() != null && !filter.getLinkedContract().isEmpty()) {
            predicates.add(criteriaBuilder.equal(root.get("linkedContract"), filter.getLinkedContract()));
        }

        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.getShowDeleted()));

        predicates.add(criteriaBuilder.equal(root.get("archived"), filter.getShowArchived()));

        return predicates;
    }
}
