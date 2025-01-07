package cz.bbn.cerberus.invoice.persistance.repository;

import com.vaadin.flow.data.provider.Query;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.invoice.dto.InvoiceFilterDto;
import cz.bbn.cerberus.invoice.factory.InvoiceFactory;
import cz.bbn.cerberus.invoice.persistance.entity.InvoiceEntity;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.permission.Permission;
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
import java.util.Objects;
import java.util.Set;

@Component
public class InvoiceDao {

    private final InvoiceRepository invoiceRepository;
    private final ListService listService;

    public InvoiceDao(InvoiceRepository invoiceRepository, ListService listService) {
        this.invoiceRepository = invoiceRepository;
        this.listService = listService;
    }

    public Page<InvoiceDto> getInvoiceDtoByContractPage(
            List<ContractDto> contractDtoList, Query<InvoiceDto, Void> query, List<Sort.Order> orderList) {
        if (contractDtoList == null || contractDtoList.isEmpty()) {
            return new PageImpl<>(new ArrayList<>(), PageRequest.of(query.getPage(),
                    query.getPageSize(), Sort.by(orderList)),
                    0);
        }
        Page<InvoiceEntity> page = invoiceRepository.findAll(getInvoiceByContractSpecification(contractDtoList),
                PageRequest.of(query.getPage(), query.getPageSize(), Sort.by(orderList)));
        List<InvoiceDto> invoiceDtoList = ConvertEntities
                .fromEntities(page.toList(), InvoiceFactory::fromEntity);
        return new PageImpl<>(invoiceDtoList, PageRequest.of(query.getPage(),
                query.getPageSize(), Sort.by(orderList)),
                page.getTotalElements());
    }


    private Specification<InvoiceEntity> getInvoiceByContractSpecification(List<ContractDto> contractDtoList) {
        return (Root<InvoiceEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getInvoiceByContractPredicateList(contractDtoList, root, criteriaBuilder)
                        .toArray(new Predicate[0]));
    }

    private List<Predicate> getInvoiceByContractPredicateList(
            List<ContractDto> contractDtoList, Root<InvoiceEntity> root, CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        predicates.add(criteriaBuilder.equal(root.get("deleted"), false));

        if (!SecurityUtils.hasCustomReadAll(DomainEnum.INVOICE_DOMAIN_NAME.getValue())) {
            predicates.add(criteriaBuilder.or(root.get("contractEntity").get("id").in(
                            SecurityUtils.getCustomReadPermission(DomainEnum.INVOICE_DOMAIN_NAME.getValue())),
                    root.get("id").in(getLongSecondaryId())));
        }

        if (!contractDtoList.isEmpty()) {
            predicates.add(root.get("contractEntity").get("id").in(
                    contractDtoList
                            .stream()
                            .map(ContractDto::getId)
                            .toList())
            );
        }

        return predicates;
    }

    public Page<InvoiceDto> getInvoiceDtoPage(InvoiceFilterDto filter) {
        Page<InvoiceEntity> page = invoiceRepository.findAll(getInvoiceSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<InvoiceDto> invoiceDtoList = ConvertEntities.fromEntities(page.toList(), InvoiceFactory::fromEntity);
        return new PageImpl<>(invoiceDtoList, PageRequest.of(filter.getPage(), filter.getSize(),
                Sort.by(filter.getOrderList())), page.getTotalElements());
    }

    private Specification<InvoiceEntity> getInvoiceSpecification(InvoiceFilterDto filter) {
        return (Root<InvoiceEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getInvoicePredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getInvoicePredicateList(InvoiceFilterDto filter,
                                                    Root<InvoiceEntity> root, CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.isShowDeleted()));

        if (!SecurityUtils.hasCustomReadAll(DomainEnum.INVOICE_DOMAIN_NAME.getValue())) {
            predicates.add(criteriaBuilder.or(root.get("contractEntity").get("id").in(
                            SecurityUtils.getCustomReadPermission(DomainEnum.INVOICE_DOMAIN_NAME.getValue())),
                    root.get("id").in(getLongSecondaryId())));
        }

        if (filter.isOnlyEditPermission()) {
            predicates.add(criteriaBuilder.or(root.get("contractEntity").get("id").in(
                            SecurityUtils.getAllowedEntityIdByDomain(Permission.INVOICE_EDIT.name(),
                                    DomainEnum.INVOICE_DOMAIN_NAME.getValue())),
                    root.get("id").in(SecurityUtils.getSecondaryOwnerSet(DomainEnum.INVOICE_DOMAIN_NAME.getValue()))));
        }

        if (filter.getPayed().getValue() != null) {
            if(filter.getPayed().getValue()){
                predicates.add(criteriaBuilder.isNotNull(root.get("paymentDate")));
            }else{
                predicates.add(criteriaBuilder.isNull(root.get("paymentDate")));
            }
        }

        if (filter.getInvoicingDateStart() != null) {
            predicates.add(criteriaBuilder.greaterThanOrEqualTo(root.get("invoicingDate"),
                    filter.getInvoicingDateStart()));
        }

        if (filter.getInvoicingDateEnd() != null) {
            predicates.add(criteriaBuilder.lessThanOrEqualTo(root.get("invoicingDate"), filter.getInvoicingDateEnd()));
        }

        if (filter.getUserDto() != null) {
            predicates.add(root.get("id").in(getIdSetByOwner(filter.getUserDto().getLogin())));
        }

        if (filter.getIssueDateStart() != null) {
            predicates.add(criteriaBuilder.greaterThanOrEqualTo(root.get("issueDate"), filter.getIssueDateStart()));
        }

        if (filter.getIssueDateEnd() != null) {
            predicates.add(criteriaBuilder.lessThanOrEqualTo(root.get("issueDate"), filter.getIssueDateEnd()));
        }

        if (filter.getInvoiceState() != null) {
            predicates.add(criteriaBuilder.equal(root.get("state"), filter.getInvoiceState().name()));
        }

        if (filter.getContractId() != null) {
            predicates.add(criteriaBuilder.equal(root.get("contractEntity").get("id"), filter.getContractId()));
        }

        if (filter.getSubjectId() != null) {
            predicates.add(root.get("id").in(getIdSetBySubject(filter.getSubjectId())));
        }

        if(filter.getTransferProtocol().getValue() != null){
            predicates.add(criteriaBuilder.equal(root.get("transferProtocol"), filter.getTransferProtocol().getValue()));
        }
        return predicates;
    }

    private Set<Long> getIdSetByOwner(String ownerId) {
        Set<String> contractDtoSet = new HashSet<>();
        for (ContractDto contractDto : listService.getContractList()) {
            if (Objects.equals(contractDto.getUserDto().getLogin(), ownerId)) {
                contractDtoSet.add(contractDto.getId());
            }
        }
        return invoiceRepository.findIdByContractIdList(contractDtoSet);
    }

    private Set<Long> getIdSetBySubject(String subjectId) {
        Set<String> contractDtoSet = new HashSet<>();
        for (ContractDto contractDto : listService.getContractList()) {
            if (contractDto.getSubjectDto() != null && Objects.equals(contractDto.getSubjectDto().getId(), subjectId)) {
                contractDtoSet.add(contractDto.getId());
            }
        }
        return invoiceRepository.findIdByContractIdList(contractDtoSet);
    }

    private Set<Long> getLongSecondaryId() {
        Set<Long> longIdSet = new HashSet<>();
        Set<String> stringIdSet = SecurityUtils.getSecondaryOwnerSet(DomainEnum.INVOICE_DOMAIN_NAME.getValue());
        for (String stringId : stringIdSet) {
            longIdSet.add(Long.parseLong(stringId));
        }
        return longIdSet;
    }
}
