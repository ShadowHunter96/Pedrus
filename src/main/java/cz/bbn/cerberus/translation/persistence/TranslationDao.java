package cz.bbn.cerberus.translation.persistence;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.translation.dto.TranslationDto;
import cz.bbn.cerberus.translation.dto.TranslationFilterDto;
import cz.bbn.cerberus.translation.factory.TranslationFactory;
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
public class TranslationDao {

    private final TranslationRepository translationRepository;

    public TranslationDao(TranslationRepository translationRepository) {
        this.translationRepository = translationRepository;
    }

    public Page<TranslationDto> findTranslationPage(TranslationFilterDto filter) {
        Page<TranslationEntity> translationPage = translationRepository.findAll(getTranslationSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<TranslationDto> translationDtoList = ConvertEntities
                .fromEntities(translationPage.toList(), TranslationFactory::fromEntity);
        return new PageImpl<>(translationDtoList, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                translationPage.getTotalElements());
    }

    private Specification<TranslationEntity> getTranslationSpecification(TranslationFilterDto filter) {
        return (Root<TranslationEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getTranslationPredicateList(filter, root, criteriaBuilder)
                        .toArray(new Predicate[0]));
    }

    private List<Predicate> getTranslationPredicateList(TranslationFilterDto filter, Root<TranslationEntity> root,
                                                        CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        if (filter.getId() != null) {
            predicates.add(criteriaBuilder.equal(root.get("id"), filter.getId()));
        }

        if (!StringUtils.isEmpty(filter.getLang())) {
            predicates.add(criteriaBuilder.like(criteriaBuilder.lower(root.get("lang")),
                    "%".concat(filter.getLang().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getValue())) {
            predicates.add(criteriaBuilder.like(criteriaBuilder.lower(root.get("value")),
                    "%".concat(filter.getValue().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getKey())) {
            predicates.add(criteriaBuilder.like(criteriaBuilder.lower(root.get("key")),
                    "%".concat(filter.getKey().toLowerCase()).concat("%")));
        }
        if (Boolean.TRUE.equals(filter.getShowEmpty())) {
            predicates.add(criteriaBuilder.isNull(root.get("value")));
        }

        return predicates;
    }
}
