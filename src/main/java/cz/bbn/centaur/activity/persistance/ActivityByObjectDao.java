package cz.bbn.cerberus.activity.persistance;

import cz.bbn.cerberus.activity.dto.ActivityByObjectDto;
import cz.bbn.cerberus.activity.dto.ActivityByObjectFilterDto;
import cz.bbn.cerberus.activity.factory.ActivityByObjectFactory;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
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
public class ActivityByObjectDao {

    private ActivityByObjectRepository activityByObjectRepository;

    public ActivityByObjectDao(ActivityByObjectRepository activityByObjectRepository) {
        this.activityByObjectRepository = activityByObjectRepository;
    }

    public Page<ActivityByObjectDto> findEnumerationPage(ActivityByObjectFilterDto filter) {
        Page<ActivityByObjectEntity> page = activityByObjectRepository.findAll(getActivityByObjectSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<ActivityByObjectDto> list = ConvertEntities
                .fromEntities(page.toList(), ActivityByObjectFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<ActivityByObjectEntity> getActivityByObjectSpecification(ActivityByObjectFilterDto filter) {
        return (Root<ActivityByObjectEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getActivityByObjectPredicateList(filter, root, criteriaBuilder)
                        .toArray(new Predicate[0]));
    }

    private List<Predicate> getActivityByObjectPredicateList(ActivityByObjectFilterDto filter,
                                                             Root<ActivityByObjectEntity> root,
                                                             CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        predicates.add(criteriaBuilder.equal(root.get("id").get("objectId"), filter.getObjectId()));
        predicates.add(criteriaBuilder.equal(root.get("id").get("objectType"), filter.getObjectType()));

        return predicates;
    }
}
