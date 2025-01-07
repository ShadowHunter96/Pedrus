package cz.bbn.cerberus.note.persistance;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.note.dto.NoteDto;
import cz.bbn.cerberus.note.dto.NoteFilterDto;
import cz.bbn.cerberus.note.factory.NoteFactory;
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
public class NoteDao {

    private final NoteRepository noteRepository;

    public NoteDao(NoteRepository noteRepository) {
        this.noteRepository = noteRepository;
    }

    public Page<NoteDto> findNoteDtoPage(NoteFilterDto filter, List<Sort.Order> orderList) {
        orderList.add(Sort.Order.desc("priority"));
        orderList.add(Sort.Order.desc("date"));

        Page<NoteEntity> page = noteRepository.findAll(getNoteSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(orderList)));
        List<NoteDto> list = ConvertEntities
                .fromEntities(page.toList(), NoteFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(orderList)),
                page.getTotalElements());
    }


    public int getNoteCountByFilter(NoteFilterDto filter) {
        return noteRepository.findAll(getNoteSpecification(filter)).size();
    }

    private Specification<NoteEntity> getNoteSpecification(NoteFilterDto filter) {
        return (Root<NoteEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getNotePredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getNotePredicateList(NoteFilterDto filter, Root<NoteEntity> root,
                                                 CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        if (filter.getNoteTypeEnum() != null && filter.getNoteTypeEnum() != NoteTypeEnum.ANY) {
            predicates.add(criteriaBuilder.equal(root.get("noteTypeEnum"), filter.getNoteTypeEnum().name()));
        } else {
            filter.setNoteTypeEnum(NoteTypeEnum.ANY);
        }

        if (filter.getEntityId() != null) {
            predicates.add(criteriaBuilder.equal(root.get("entityId"), filter.getEntityId()));
        } else {
            Set<String> allowedSet = getAllowedEntitySet(filter.getNoteTypeEnum().getObjectName(), filter.getPermission());
            predicates.add(root.get("entityId").in(allowedSet));
        }

        if (!StringUtils.isEmpty(filter.getText())) {
            predicates.add(criteriaBuilder.or(
                    criteriaBuilder.like(criteriaBuilder.lower(root.get("note")),
                            "%".concat(filter.getText().toLowerCase()).concat("%")),
                    criteriaBuilder.like(criteriaBuilder.lower(root.get("userEntity").get("name")),
                            "%".concat(filter.getText().toLowerCase()).concat("%"))));
        }

        if (filter.getCreatedBy() != null) {
            predicates.add(criteriaBuilder.equal(root.get("userEntity").get("id"), filter.getCreatedBy().getId()));
        }

        if (filter.getCreatedFrom() != null) {
            predicates.add(criteriaBuilder.greaterThanOrEqualTo(root.get("date"), filter.getCreatedFrom()));
        }

        if (filter.getCreatedTo() != null) {
            predicates.add(criteriaBuilder.lessThanOrEqualTo(root.get("date"), filter.getCreatedTo()));
        }

        predicates.add(criteriaBuilder.equal(root.get("archived"), Boolean.TRUE.equals(filter.getShowArchived())));

        return predicates;
    }

    public static Set<String> getAllowedEntitySet(String domain, Set<String> permission) {
        if (permission == null || domain == null) {
            return new HashSet<>();
        }
        if (NoteTypeEnum.ANY.getObjectName().equals(domain)) {
            return SecurityUtils.getAllowedEntityIdSet(permission);
        } else {
            return SecurityUtils.getAllowedEntityIdByDomain(permission, domain);
        }
    }
}
