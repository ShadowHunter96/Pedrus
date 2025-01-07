package cz.bbn.cerberus.document.persistance.repository;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.document.DocumentObjectEnum;
import cz.bbn.cerberus.document.dto.DocumentDto;
import cz.bbn.cerberus.document.dto.DocumentFilterDto;
import cz.bbn.cerberus.document.factory.DocumentFactory;
import cz.bbn.cerberus.document.persistance.entity.DocumentByObjectEntity;
import cz.bbn.cerberus.document.persistance.entity.DocumentEntity;
import cz.bbn.cerberus.document.persistance.entity.DocumentSimpleEntity;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.util.ArrayList;
import java.util.List;

@Component
public class DocumentDao {

    private static final String DOCUMENT_BY_OBJECT_ENTITY = "documentByObjectEntity";

    private final DocumentSimpleRepository documentSimpleRepository;

    public DocumentDao(DocumentSimpleRepository documentSimpleRepository) {
        this.documentSimpleRepository = documentSimpleRepository;
    }


    public Page<DocumentDto> findDocumentDtoPage(DocumentFilterDto filter) {
        Page<DocumentSimpleEntity> page = documentSimpleRepository.findAll(getDocumentSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<DocumentDto> list = ConvertEntities
                .fromEntities(page.toList(), DocumentFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<DocumentSimpleEntity> getDocumentSpecification(DocumentFilterDto filter) {
        return (Root<DocumentSimpleEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getDocumentPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getDocumentPredicateList(DocumentFilterDto filter, Root<DocumentSimpleEntity> root,
                                                     CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();

        if (!StringUtils.isEmpty(filter.getName())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("name")), "%".concat(filter.getName().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getFileType())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("fileType")),
                    "%".concat(filter.getFileType().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getDocumentType())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("documentType")), filter.getDocumentType().toLowerCase()));
        }

        if (filter.getObjectType() == DocumentObjectEnum.ALL && !StringUtils.isEmpty(filter.getObjectId())) {
            Join<DocumentEntity, DocumentByObjectEntity> join = root.join(DOCUMENT_BY_OBJECT_ENTITY);
            predicates.add(criteriaBuilder.equal(join.get("id").get("objectId"), filter.getObjectId()));

        } else if (filter.getObjectType() != null && filter.getObjectType() != DocumentObjectEnum.ALL
                && !StringUtils.isEmpty(filter.getObjectId())) {
            Join<DocumentEntity, DocumentByObjectEntity> join = root.join(DOCUMENT_BY_OBJECT_ENTITY);
            predicates.add(criteriaBuilder.equal(join.get("id").get("objectId"), filter.getObjectId()));
            predicates.add(criteriaBuilder.equal(join.get("id").get("objectType"), filter.getObjectType()));

        } else if (filter.getObjectType() != null && filter.getObjectType() != DocumentObjectEnum.ALL
                && StringUtils.isEmpty(filter.getObjectId())) {
            Join<DocumentEntity, DocumentByObjectEntity> join = root.join(DOCUMENT_BY_OBJECT_ENTITY);
            predicates.add(criteriaBuilder.equal(join.get("id").get("objectType"), filter.getObjectType()));
        }

        if (filter.isShowOnlyUnlinked()) {
            predicates.add(criteriaBuilder.isEmpty(root.get(DOCUMENT_BY_OBJECT_ENTITY)));
        }

        if (filter.isShowOnlyDeleted()) {
            predicates.add(criteriaBuilder.equal(root.get("deleted"), Boolean.TRUE));
        } else {
            predicates.add(criteriaBuilder.equal(root.get("deleted"), Boolean.FALSE));
        }

        return predicates;
    }
}
