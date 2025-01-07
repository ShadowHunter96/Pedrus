package cz.bbn.cerberus.labelsubject.persistance;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.labelsubject.dto.LabelSubjectDto;
import cz.bbn.cerberus.labelsubject.dto.LabelSubjectFilterDto;
import cz.bbn.cerberus.labelsubject.factory.LabelSubjectFactory;
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
public class LabelSubjectDao {

    private final LabelSubjectRepository labelSubjectRepository;

    public LabelSubjectDao(LabelSubjectRepository labelSubjectRepository) {
        this.labelSubjectRepository = labelSubjectRepository;
    }

    public Page<LabelSubjectDto> findLabelPage(LabelSubjectFilterDto filter) {
        Page<LabelSubjectEntity> page = labelSubjectRepository.findAll(getLabelSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<LabelSubjectDto> list = ConvertEntities
                .fromEntities(page.toList(), LabelSubjectFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<LabelSubjectEntity> getLabelSpecification(LabelSubjectFilterDto filter) {
        return (Root<LabelSubjectEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getLabelPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getLabelPredicateList(LabelSubjectFilterDto filter, Root<LabelSubjectEntity> root,
                                                  CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(criteriaBuilder.equal(root.get("subjectId"), filter.getSubjectId()));
        return predicates;
    }
}
