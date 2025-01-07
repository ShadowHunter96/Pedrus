package cz.bbn.cerberus.task.persitance.repository;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.task.dto.TaskDto;
import cz.bbn.cerberus.task.dto.TaskFilterDto;
import cz.bbn.cerberus.task.dto.TaskState;
import cz.bbn.cerberus.task.factory.TaskFactory;
import cz.bbn.cerberus.task.persitance.entity.TaskEntity;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Component
public class TaskDao {

    private final TaskRepository taskRepository;

    public TaskDao(TaskRepository taskRepository) {
        this.taskRepository = taskRepository;
    }

    public Page<TaskDto> findTaskDtoPage(TaskFilterDto filter) {
        Page<TaskEntity> page = taskRepository.findAll(getTaskSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<TaskDto> list = ConvertEntities
                .fromEntities(page.toList(), TaskFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    public int getTaskCount(TaskFilterDto filter) {
        return taskRepository.findAll(getTaskSpecification(filter)).size();
    }

    private Specification<TaskEntity> getTaskSpecification(TaskFilterDto filter) {
        return (Root<TaskEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            query.distinct(true);
            return criteriaBuilder.and(getTaskPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
        };
    }

    private List<Predicate> getTaskPredicateList(TaskFilterDto filter, Root<TaskEntity> root,
                                                 CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        if (filter.getId() != null) {
            predicates.add(criteriaBuilder.equal(root.get("id"), filter.getId()));
        }

        if (!StringUtils.isEmpty(filter.getSearch())) {
            predicates.add(criteriaBuilder.or(
                    criteriaBuilder.like(
                            criteriaBuilder.lower(root.get("name")),
                            "%".concat(filter.getSearch().toLowerCase()).concat("%")
                    ), criteriaBuilder.like(
                            criteriaBuilder.lower(root.get("description")),
                            "%".concat(filter.getSearch().toLowerCase()).concat("%")
                    )
            ));
        }

        if (Boolean.TRUE.equals(filter.getShowAssignedToMe())) {
            predicates.add(criteriaBuilder.or(criteriaBuilder.equal(root.join("taskUserEntitySet", JoinType.LEFT)
                            .get("id").get("userEntity").get("id"), SecurityUtils.getCurrentUserId()),
                    root.join("taskRoleEntitySet", JoinType.LEFT).get("id").get("roleEntity").get("id")
                            .in(SecurityUtils.getCurrentUser().getActiveRoleSet())));
        }

        if (Boolean.TRUE.equals(filter.getShowMeSolver())) {
            predicates.add(criteriaBuilder.equal(root.get("assignee").get("id"), SecurityUtils.getCurrentUserId()));
        }

        if (filter.getStateSet() != null && !filter.getStateSet().isEmpty()) {
            Set<String> stateStringSet = new HashSet<>();
            for (TaskState state : filter.getStateSet()) {
                stateStringSet.add(state.name());
            }
            predicates.add(root.get("state").in(stateStringSet));
        }

        if (!SecurityUtils.hasPermission(Permission.TASK_LIST_VIEW)) {
            predicates.add(criteriaBuilder.or(
                    criteriaBuilder.equal(root.get("userEntity").get("id"), SecurityUtils.getCurrentUserId()),
                    criteriaBuilder.equal(root.join("taskUserEntitySet", JoinType.LEFT)
                            .get("id").get("userEntity").get("id"), SecurityUtils.getCurrentUserId()),
                    root.get("objectId").in(getAllowedEntityId()),
                    root.join("taskRoleEntitySet", JoinType.LEFT).get("id").get("roleEntity").get("id")
                            .in(SecurityUtils.getCurrentUser().getActiveRoleSet())));
        }

        if (filter.getAssignedUsers() != null && !filter.getAssignedUsers().isEmpty()) {
            predicates.add(root.join("taskUserEntitySet", JoinType.LEFT)
                    .get("id").get("userEntity").get("id").in(filter.getAssignedUsers()));
        }

        if (filter.getOwner() != null) {
            predicates.add(criteriaBuilder.equal(root.get("userEntity").get("id"), filter.getOwner()));
        }

        if (filter.getObjectType() != null && filter.getSubjectId() == null
                && filter.getObjectId() != null && !filter.getObjectId().isEmpty()) {
            if (filter.getObjectType() != ObjectType.SUBJECT) {
                predicates.add(criteriaBuilder.equal(root.get("objectType"), filter.getObjectType()));
                predicates.add(criteriaBuilder.equal(root.get("objectId"), filter.getObjectId()));
            } else {
                predicates.add(criteriaBuilder.equal(root.get("subjectId"), filter.getObjectId()));
            }
        } else if (filter.getObjectType() != null && filter.getObjectType() != ObjectType.ANY) {
            predicates.add(criteriaBuilder.equal(root.get("objectType"), filter.getObjectType()));
            if (filter.getObjectId() != null && !filter.getObjectId().isEmpty()) {
                predicates.add(criteriaBuilder.equal(root.get("objectId"), filter.getSubjectId()));
            }
        }

        if (filter.getDateFrom() != null) {
            predicates.add(criteriaBuilder.greaterThanOrEqualTo(
                    root.get("dueDate"), filter.getDateFrom()));
        }

        if (filter.getDateTo() != null) {
            predicates.add(criteriaBuilder.lessThanOrEqualTo(
                    root.get("dueDate"), filter.getDateTo()));
        }

        predicates.add(criteriaBuilder.or(
                criteriaBuilder.notEqual(root.get("deleted"), true), criteriaBuilder.isNull(root.get("deleted"))));

        return predicates;
    }

    private Set<String> getAllowedEntityId() {
        Set<String> permissionSet = new HashSet<>();
        for (Permission permission : Permission.values()) {
            if (permission.name().contains("TASK_VIEW")) {
                permissionSet.add(permission.name());
            }
        }
        return SecurityUtils.getAllowedEntityIdSet(permissionSet);
    }
}
