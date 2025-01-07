package cz.bbn.cerberus.taskfollowing.persistance;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.taskfollowing.dto.TaskFollowingDto;
import cz.bbn.cerberus.taskfollowing.dto.TaskFollowingFilterDto;
import cz.bbn.cerberus.taskfollowing.factory.TaskFollowingFactory;
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
public class TaskFollowingDao {

    private final TaskFollowingRepository taskFollowingRepository;

    public TaskFollowingDao(TaskFollowingRepository taskFollowingRepository) {
        this.taskFollowingRepository = taskFollowingRepository;
    }

    public Page<TaskFollowingDto> findTaskFollowingDtoPage(TaskFollowingFilterDto filter) {
        Page<TaskFollowingEntity> page = taskFollowingRepository.findAll(getDocumentSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<TaskFollowingDto> list = ConvertEntities
                .fromEntities(page.toList(), TaskFollowingFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<TaskFollowingEntity> getDocumentSpecification(TaskFollowingFilterDto filter) {
        return (Root<TaskFollowingEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getDocumentPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getDocumentPredicateList(TaskFollowingFilterDto filter, Root<TaskFollowingEntity> root,
                                                     CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        if (!StringUtils.isEmpty(filter.getFollowingUserName())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("id").get("followingUserEntity").get("name")),
                    "%".concat(filter.getFollowingUserName().toLowerCase()).concat("%")));
        }
        return predicates;
    }
}
