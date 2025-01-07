package cz.bbn.cerberus.contract.persistence.repository;

import cz.bbn.cerberus.area.dto.AreaDto;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.contract.dto.ContractFilterDto;
import cz.bbn.cerberus.contract.dto.ContractInternalType;
import cz.bbn.cerberus.contract.factory.ContractFactory;
import cz.bbn.cerberus.contract.persistence.entity.ContractEntity;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.technology.dto.TechnologyDto;
import org.apache.commons.collections4.CollectionUtils;
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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Component
public class ContractDao {

    private final ContractRepository contractRepository;

    public ContractDao(ContractRepository contractRepository) {
        this.contractRepository = contractRepository;
    }

    public List<ContractDto> getMyInvoiceEditContractList() {
        return ConvertEntities.fromEntities(contractRepository.findAll(getInvoiceContractSpecification()),
                ContractFactory::fromEntity);
    }

    public Page<ContractDto> findContractPage(ContractFilterDto filter) {
        Page<ContractEntity> contractEntityPage = contractRepository.findAll(getContractSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<ContractDto> contractDtoList = ConvertEntities
                .fromEntities(contractEntityPage.toList(), ContractFactory::fromEntity);
        return new PageImpl<>(contractDtoList, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                contractEntityPage.getTotalElements());
    }


    private Specification<ContractEntity> getContractSpecification(ContractFilterDto filter) {
        return (Root<ContractEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getContractPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private Specification<ContractEntity> getInvoiceContractSpecification() {
        return (Root<ContractEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getInvoiceContractPredicateList(root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getInvoiceContractPredicateList(
            Root<ContractEntity> root, CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(root.get("id").in(SecurityUtils.getAllowedEntityIdByDomain(
                Permission.INVOICE_EDIT.name(), DomainEnum.INVOICE_DOMAIN_NAME.getValue())));
        predicates.add(criteriaBuilder.equal(root.get("deleted"), false));
        return predicates;
    }

    private List<Predicate> getContractPredicateList(
            ContractFilterDto filter, Root<ContractEntity> root, CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        if (filter.isOnlyEditPermission()) {
            predicates.add(root.get("id").in(SecurityUtils.getAllowedEntityIdByDomain(Permission.CONTRACT_EDIT.name(),
                    DomainEnum.CONTRACT_DOMAIN_NAME.getValue())));
        } else if (!hasSalesList(filter) && !hasBoList(filter)
                && !SecurityUtils.hasCustomReadAll(DomainEnum.CONTRACT_DOMAIN_NAME.getValue())) {
            predicates.add(root.get("id").in(SecurityUtils.getCustomReadPermission(
                    DomainEnum.CONTRACT_DOMAIN_NAME.getValue())));
        }


        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.isShowDeleted()));

        if (filter.getCustomerDto() != null) {
            if (ContractInternalType.SALES == filter.getContractInternalType()) {
                predicates.add(criteriaBuilder.or(
                        criteriaBuilder.equal(root.get("subject").get("id"), filter.getCustomerDto().getId()),
                        criteriaBuilder.equal(root.get("contractParty").get("id"), filter.getCustomerDto().getId())));
            } else {
                predicates.add(criteriaBuilder.equal(root.get("subject").get("id"), filter.getCustomerDto().getId()));
            }
        }

        if (!CollectionUtils.isEmpty(filter.getContractState())) {
            predicates.add(root.get("contractState").get("id").in(filter.getContractState().stream()
                    .map(EnumerationDto::getId)
                    .toList()));
        }

        if (filter.getContractPartyDto() != null) {
            predicates.add(criteriaBuilder.equal(
                    root.get("contractParty").get("id"), filter.getContractPartyDto().getId()));
        }

        if (filter.getContractTypeDto() != null) {
            predicates.add(criteriaBuilder.equal(root.get("type").get("id"), filter.getContractTypeDto().getId()));
        }

        if (!StringUtils.isEmpty(filter.getId())) {
            predicates.add(AppUtils.stripAccentLike(criteriaBuilder, root.get("id"), filter.getId()));
        }

        if (!StringUtils.isEmpty(filter.getName())) {
            predicates.add(AppUtils.stripAccentLike(criteriaBuilder, root.get("name"), filter.getName()));
        }

        if (filter.isShowEmptyValidityStart()) {
            predicates.add(criteriaBuilder.isNull(root.get("validityStart")));
        }

        if (filter.getUserDto() != null) {
            predicates.add(criteriaBuilder.equal(root.get("userEntity").get("id"), filter.getUserDto().getId()));
        }

        if (filter.getParentContractId() != null) {
            predicates.add(criteriaBuilder.equal(
                    root.get("connectedContract").get("id"), filter.getParentContractId()));
        }

        if (filter.getContractState() != null && !filter.getContractState().isEmpty()) {
            predicates.add(root.get("contractState").get("id").in(filter.getContractState().stream()
                    .map(EnumerationDto::getId)
                    .toList()));
        }

        if (filter.getValidityStartFrom() != null) {
            predicates.add(criteriaBuilder.greaterThanOrEqualTo(
                    root.get("validityStart"), filter.getValidityStartFrom()));
        }

        if (filter.getValidityStartTo() != null) {
            predicates.add(criteriaBuilder.lessThanOrEqualTo(root.get("validityStart"), filter.getValidityStartTo()));
        }

        if (filter.getEndDateFrom() != null) {
            predicates.add(criteriaBuilder.greaterThanOrEqualTo(root.get("endContract"), filter.getEndDateFrom()));
        }

        if (filter.getEndDateTo() != null) {
            predicates.add(criteriaBuilder.lessThanOrEqualTo(root.get("endContract"), filter.getEndDateTo()));
        }

        if (filter.getEffectiveStartFrom() != null) {
            predicates.add(criteriaBuilder.greaterThanOrEqualTo(
                    root.get("effectStart"), filter.getEffectiveStartFrom()));
        }

        if (filter.getEffectiveStartTo() != null) {
            predicates.add(criteriaBuilder.lessThanOrEqualTo(root.get("effectStart"), filter.getEffectiveStartTo()));
        }

        if (!CollectionUtils.isEmpty(filter.getAreaDtoSet())) {
            predicates.add(root.join("areaTechnologySignEntitySet").get("areaEntity")
                    .get("id").in(filter.getAreaDtoSet().stream()
                            .map(AreaDto::getId)
                            .toList()));
        }

        if (!CollectionUtils.isEmpty(filter.getTechnologyDtoSet())) {
            predicates.add(root.join("areaTechnologySignEntitySet").get("technologyEntity")
                    .get("id").in(filter.getTechnologyDtoSet().stream()
                            .map(TechnologyDto::getId)
                            .toList()));
        }

        if (filter.getContractInternalType() != null) {
            if (filter.getContractInternalType() == ContractInternalType.SALES) {
                predicates.add(criteriaBuilder.or(
                        criteriaBuilder.equal(root.get("contractInternalType"), ContractInternalType.SALES.name()),
                        criteriaBuilder.equal(root.get("contractInternalType"), ContractInternalType.SUPPLIER.name())));
            } else {
                predicates.add(criteriaBuilder.equal(
                        root.get("contractInternalType"), filter.getContractInternalType().name()));
            }
        }

        if (filter.getTypeSet() != null) {
            Set<String> typeStrSet = new HashSet<>();
            for (ContractInternalType type : filter.getTypeSet()) {
                typeStrSet.add(type.name());
            }
            predicates.add(root.get("contractInternalType").in(typeStrSet));
        }

        return predicates;
    }

    private boolean hasSalesList(ContractFilterDto filterDto) {
        return SecurityUtils.hasPermission(Permission.SALES_CONTRACT_LIST_VIEW)
                && ContractInternalType.SALES == filterDto.getContractInternalType();
    }

    private boolean hasBoList(ContractFilterDto filterDto) {
        return SecurityUtils.hasPermission(Permission.BO_CONTRACT_LIST_VIEW)
                && ContractInternalType.OPERATIONAL == filterDto.getContractInternalType();
    }
}
