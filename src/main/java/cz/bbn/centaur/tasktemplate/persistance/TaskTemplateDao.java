package cz.bbn.cerberus.tasktemplate.persistance;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.tasktemplate.dto.TaskTemplateDto;
import cz.bbn.cerberus.tasktemplate.dto.TaskTemplateFilterDto;
import cz.bbn.cerberus.tasktemplate.factory.TaskTemplateFactory;
import cz.bbn.cerberus.tasktemplate.persistance.entity.TaskTemplateEntity;
import cz.bbn.cerberus.tasktemplate.persistance.repository.TaskTemplateRepository;
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
public class TaskTemplateDao {

    private final TaskTemplateRepository taskTemplateRepository;

    public TaskTemplateDao(TaskTemplateRepository taskTemplateRepository) {
        this.taskTemplateRepository = taskTemplateRepository;
    }

    public Page<TaskTemplateDto> findTaskTemplateDtoPage(TaskTemplateFilterDto filter) {
        Page<TaskTemplateEntity> page = taskTemplateRepository.findAll(getTaskTemplateSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<TaskTemplateDto> list = ConvertEntities
                .fromEntities(page.toList(), TaskTemplateFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<TaskTemplateEntity> getTaskTemplateSpecification(TaskTemplateFilterDto filter) {
        return (Root<TaskTemplateEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            query.distinct(true);
            return criteriaBuilder.and(getTaskTemplatePredicateList(filter, root, criteriaBuilder)
                    .toArray(new Predicate[0]));
        };
    }

    private List<Predicate> getTaskTemplatePredicateList(TaskTemplateFilterDto filter, Root<TaskTemplateEntity> root,
                                                         CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        if (filter.getId() != null) {
            predicates.add(criteriaBuilder.equal(root.get("id"), filter.getId()));
        }

        if (!StringUtils.isEmpty(filter.getSearch())) {
            predicates.add(criteriaBuilder.or(criteriaBuilder.like(
                            criteriaBuilder.lower(root.get("name")), "%".concat(filter.getSearch().toLowerCase()).concat("%")),
                    criteriaBuilder.like(criteriaBuilder.lower(root.get("description")),
                            "%".concat(filter.getSearch().toLowerCase()).concat("%"))));
        }

        if (filter.getOwner() != null) {
            predicates.add(criteriaBuilder.equal(root.get("userEntity").get("id"), filter.getOwner()));
        }

        if (filter.getRole() != null) {
            predicates.add(criteriaBuilder.equal(root.get("allowedRole").get("id"), filter.getRole()));
        }

        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.isShowDeleted()));

        if (filter.isShowMine()) {
            predicates.add(criteriaBuilder.equal(root.get("userEntity").get("id"), SecurityUtils.getCurrentUserId()));
        }

        /*
        predicates.add(criteriaBuilder.or(
                criteriaBuilder.isNull(root.get("objectId")),
                root.get("objectId").in(getAllowedEntityId())));
         */

        return predicates;
    }

    private Set<String> getAllowedEntityId() {
        Set<String> permissionSet = new HashSet<>();
        for (Permission permission : Permission.values()) {
            if (permission.name().contains("TASK_TEMPLATE_VIEW")) {
                permissionSet.add(permission.name());
            }
        }
        return SecurityUtils.getAllowedEntityIdSet(permissionSet);
    }
}
