package cz.bbn.cerberus.project.persistance;

import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.project.dto.ProjectFilterDto;
import cz.bbn.cerberus.project.factory.ProjectFactory;
import cz.bbn.cerberus.project.persistance.entity.ProjectEntity;
import cz.bbn.cerberus.project.persistance.repository.ProjectRepository;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.util.ArrayList;
import java.util.List;

@Component
public class ProjectDao {

    private final ProjectRepository projectRepository;

    public ProjectDao(ProjectRepository projectRepository) {
        this.projectRepository = projectRepository;
    }

    public List<ProjectDto> findProjectList() {
        ProjectFilterDto filterDto = new ProjectFilterDto();
        filterDto.setShowDeleted(false);
        List<ProjectEntity> projectDtoList = projectRepository.findAll(getProjectSpecification(filterDto));
        return ConvertEntities.fromEntities(projectDtoList, ProjectFactory::fromEntity);
    }

    private Specification<ProjectEntity> getProjectSpecification(ProjectFilterDto filter) {
        return (Root<ProjectEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getProjectPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getProjectPredicateList(ProjectFilterDto filter, Root<ProjectEntity> root,
                                                    CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        if (!SecurityUtils.hasPermission(Permission.PROJECT_LIST_VIEW)
                && !SecurityUtils.hasCustomReadAll(DomainEnum.PROJECT_DOMAIN_NAME.getValue())) {
            predicates.add(root.get("id").in(SecurityUtils.getCustomReadPermission(
                    DomainEnum.PROJECT_DOMAIN_NAME.getValue())));
        }
        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.isShowDeleted()));

        return predicates;
    }
}
