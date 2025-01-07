package cz.bbn.cerberus.tasktype.persistance;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.tasktype.dto.TaskTypeDto;
import cz.bbn.cerberus.tasktype.factory.TaskTypeFactory;
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
public class TaskTypeDao {

    private final TaskTypeRepository taskTypeRepository;

    public TaskTypeDao(TaskTypeRepository taskTypeRepository) {
        this.taskTypeRepository = taskTypeRepository;
    }

    public Page<TaskTypeDto> findTaskTypeDtoPage(int page, int size, List<Sort.Order> sortList) {
        Page<TaskTypeEntity> taskEntityPage = taskTypeRepository.findAll(getTaskTypeSpecification(),
                PageRequest.of(page, size, Sort.by(sortList)));
        List<TaskTypeDto> list = ConvertEntities
                .fromEntities(taskEntityPage.toList(), TaskTypeFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(page,
                size, Sort.by(sortList)),
                taskEntityPage.getTotalElements());
    }

    private Specification<TaskTypeEntity> getTaskTypeSpecification() {
        return (Root<TaskTypeEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getTaskTypePredicateList(root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getTaskTypePredicateList(Root<TaskTypeEntity> root,
                                                     CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        predicates.add(criteriaBuilder.equal(root.get("archived"), false));

        return predicates;
    }
}
