package cz.bbn.cerberus.virtualserver.persistance;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerDto;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerFilterDto;
import cz.bbn.cerberus.virtualserver.factory.VirtualServerFactory;
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
public class VirtualServerDao {

    private final VirtualServerRepository virtualServerRepository;

    public VirtualServerDao(VirtualServerRepository virtualServerRepository) {
        this.virtualServerRepository = virtualServerRepository;
    }

    public Page<VirtualServerDto> findVirtualServerPage(VirtualServerFilterDto filter) {
        Page<VirtualServerEntity> virtualServerDtoPage = virtualServerRepository.findAll(
                getVirtualServerSpecification(filter), PageRequest.of(filter.getPage(), filter.getSize(),
                        Sort.by(filter.getOrderList())));
        List<VirtualServerDto> virtualServerDtoList = ConvertEntities
                .fromEntities(virtualServerDtoPage.toList(), VirtualServerFactory::fromEntity);
        return new PageImpl<>(virtualServerDtoList, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                virtualServerDtoPage.getTotalElements());
    }

    private Specification<VirtualServerEntity> getVirtualServerSpecification(VirtualServerFilterDto filter) {
        return (Root<VirtualServerEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getVirtualServerPredicateList(filter, root, criteriaBuilder)
                        .toArray(new Predicate[0]));
    }

    private List<Predicate> getVirtualServerPredicateList(VirtualServerFilterDto filter, Root<VirtualServerEntity> root,
                                                          CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.getShowDeleted()));

        if (filter.getId() != null) {
            predicates.add(criteriaBuilder.equal(root.get("id"), filter.getId()));
        }

        if (!StringUtils.isEmpty(filter.getName())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("name")), "%".concat(filter.getName().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getOs())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("os")), "%".concat(filter.getOs().toLowerCase()).concat("%")));
        }

        if (filter.getCpu() != null) {
            predicates.add(criteriaBuilder.equal(root.get("cpu"), filter.getCpu()));
        }

        if (filter.getCores() != null) {
            predicates.add(criteriaBuilder.equal(root.get("cores"), filter.getCores()));
        }

        if (filter.getRam() != null) {
            predicates.add(criteriaBuilder.equal(root.get("ram"), filter.getRam()));
        }

        if (!StringUtils.isEmpty(filter.getIp())) {
            predicates.add(criteriaBuilder.like(root.get("ip"), "%".concat(filter.getIp()).concat("%")));
        }

        if (filter.getOwner() != null) {
            predicates.add(criteriaBuilder.equal(root.get("owner").get("id"), filter.getOwner().getId()));
        }

        if (filter.getStatus() != null) {
            predicates.add(criteriaBuilder.equal(root.get("status"), filter.getStatus().name()));
        }

        return predicates;
    }
}
