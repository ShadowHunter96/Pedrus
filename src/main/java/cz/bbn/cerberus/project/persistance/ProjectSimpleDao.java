package cz.bbn.cerberus.project.persistance;

import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.dto.ProjectFilterDto;
import cz.bbn.cerberus.project.dto.ProjectSimpleDto;
import cz.bbn.cerberus.project.factory.ProjectSimpleFactory;
import cz.bbn.cerberus.project.persistance.entity.ProjectSimpleEntity;
import cz.bbn.cerberus.project.persistance.repository.ProjectSimpleRepository;
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
public class ProjectSimpleDao {

    private final ProjectSimpleRepository projectSimpleRepository;

    public ProjectSimpleDao(ProjectSimpleRepository projectSimpleRepository) {
        this.projectSimpleRepository = projectSimpleRepository;
    }

    public Page<ProjectSimpleDto> findProjectPage(ProjectFilterDto filter) {
        Page<ProjectSimpleEntity> projectDtoPage = projectSimpleRepository.findAll(getProjectSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<ProjectSimpleDto> projectDtoList = ConvertEntities
                .fromEntities(projectDtoPage.toList(), ProjectSimpleFactory::fromEntity);
        return new PageImpl<>(projectDtoList, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                projectDtoPage.getTotalElements());
    }

    private Specification<ProjectSimpleEntity> getProjectSpecification(ProjectFilterDto filter) {
        return (Root<ProjectSimpleEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getProjectPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getProjectPredicateList(ProjectFilterDto filter, Root<ProjectSimpleEntity> root,
                                                    CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        if (filter.isOnlyEditPermission()) {
            predicates.add(root.get("id").in(SecurityUtils.getAllowedEntityIdByDomain(Permission.PROJECT_EDIT.name(),
                    DomainEnum.PROJECT_DOMAIN_NAME.getValue())));
        } else if (!SecurityUtils.hasPermission(Permission.PROJECT_LIST_VIEW)
                && !SecurityUtils.hasCustomReadAll(DomainEnum.PROJECT_DOMAIN_NAME.getValue())) {
            predicates.add(root.get("id").in(SecurityUtils.getCustomReadPermission(
                    DomainEnum.PROJECT_DOMAIN_NAME.getValue())));
        }
        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.isShowDeleted()));

        if (!StringUtils.isEmpty(filter.getId())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("id")), "%".concat(filter.getId().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getName())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("name")), "%".concat(filter.getName().toLowerCase()).concat("%")));
        }


        if (!StringUtils.isEmpty(filter.getSubject())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("subjectId")),
                    "%".concat(filter.getSubject().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getContract())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("contract")),
                    "%".concat(filter.getContract().toLowerCase()).concat("%")));
        }

        if (filter.getUserDto() != null && filter.getUserDto().getId() != 0) {
            predicates.add(criteriaBuilder.equal(root.get("userEntity").get("id"), filter.getUserDto().getId()));
        }

        if (filter.getState() != null) {
            predicates.add(criteriaBuilder.equal(root.get("projectState"), filter.getState()));
        }

        return predicates;
    }
}
