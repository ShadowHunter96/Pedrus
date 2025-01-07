package cz.bbn.cerberus.taskschedule.persistance;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.taskschedule.dto.TaskScheduleDto;
import cz.bbn.cerberus.taskschedule.dto.TaskScheduleFilterDto;
import cz.bbn.cerberus.taskschedule.factory.TaskScheduleFactory;
import cz.bbn.cerberus.taskschedule.persistance.entity.TaskScheduleEntity;
import cz.bbn.cerberus.taskschedule.persistance.repository.TaskScheduleRepository;
import cz.bbn.cerberus.user.dto.UserDto;
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
public class TaskScheduleDao {

    private final TaskScheduleRepository taskScheduleRepository;

    public TaskScheduleDao(TaskScheduleRepository taskScheduleRepository) {
        this.taskScheduleRepository = taskScheduleRepository;
    }

    public Page<TaskScheduleDto> findTaskScheduleDtoPage(TaskScheduleFilterDto filter) {
        Page<TaskScheduleEntity> page = taskScheduleRepository.findAll(getTaskScheduleSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<TaskScheduleDto> list = ConvertEntities
                .fromEntities(page.toList(), TaskScheduleFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<TaskScheduleEntity> getTaskScheduleSpecification(TaskScheduleFilterDto filter) {
        return (Root<TaskScheduleEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            query.distinct(true);
            return criteriaBuilder.and(getTaskSchedulePredicateList(filter, root, criteriaBuilder)
                    .toArray(new Predicate[0]));
        };
    }

    private List<Predicate> getTaskSchedulePredicateList(TaskScheduleFilterDto filter, Root<TaskScheduleEntity> root,
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

        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.isShowDeleted()));

        if (filter.isShowMine()) {
            predicates.add(criteriaBuilder.equal(root.get("userEntity").get("id"), SecurityUtils.getCurrentUserId()));
        }

        if (filter.getAssignedUserSet() != null && !filter.getAssignedUserSet().isEmpty()) {
            Set<Long> assignedUserSet = new HashSet<>();
            for (UserDto userDto : filter.getAssignedUserSet()) {
                assignedUserSet.add(userDto.getId());
            }
            predicates.add(root.join("taskScheduleUserEntitySet", JoinType.LEFT)
                    .get("id").get("userEntity").get("id").in(assignedUserSet));
        }

        if (filter.isShowMine()) {
            Set<Long> assignedUserSet = new HashSet<>();
            assignedUserSet.add(SecurityUtils.getCurrentUserId());
            predicates.add(root.join("taskScheduleUserEntitySet", JoinType.LEFT)
                    .get("id").get("userEntity").get("id").in(assignedUserSet));
        }

        if (filter.getObjectType() != null) {
            predicates.add(criteriaBuilder.equal(root.get("objectType"), filter.getObjectType().name()));
        }

        if (filter.getObjectId() != null && !filter.getObjectId().isEmpty()) {
            predicates.add(criteriaBuilder.like(criteriaBuilder.lower(root.get("objectId")),
                    "%".concat(filter.getObjectId().toLowerCase()).concat("%")));
        }

        if (filter.getFrequency() != null) {
            predicates.add(criteriaBuilder.equal(root.get("frequency"), filter.getFrequency().name()));
        }

        if (filter.getTaskType() != null) {
            predicates.add(criteriaBuilder.equal(root.get("taskType").get("id"), filter.getTaskType().getId()));
        }

        return predicates;
    }
}
