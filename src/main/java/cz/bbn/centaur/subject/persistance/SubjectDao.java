package cz.bbn.cerberus.subject.persistance;

import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.subject.dto.SubjectFilterDto;
import cz.bbn.cerberus.subject.dto.SubjectType;
import cz.bbn.cerberus.subject.factory.SubjectFactory;
import cz.bbn.cerberus.user.dto.UserDto;
import org.apache.commons.collections4.CollectionUtils;
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
public class SubjectDao {

    private final SubjectRepository subjectRepository;

    public SubjectDao(SubjectRepository subjectRepository) {
        this.subjectRepository = subjectRepository;
    }

    public List<SubjectDto> getMySubjects() {
        SubjectFilterDto filter = new SubjectFilterDto();
        filter.setShowDeleted(false);
        return ConvertEntities.fromEntities(
                subjectRepository.findAll(getSubjectSpecification(filter)), SubjectFactory::fromEntity);
    }

    public List<SubjectDto> getAllowedCustomers() {
        SubjectFilterDto filter = new SubjectFilterDto();
        filter.setShowDeleted(false);
        Set<SubjectType> typeSet = new HashSet<>();
        typeSet.add(SubjectType.CUSTOMER);
        filter.setSubjectTypeSet(typeSet);
        return ConvertEntities.fromEntities(
                subjectRepository.findAll(getSubjectSpecification(filter)), SubjectFactory::fromEntity);
    }

    public Page<SubjectDto> findSubjectPage(SubjectFilterDto filter) {
        Page<SubjectEntity> subjectEntityPage = subjectRepository.findAll(getSubjectSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<SubjectDto> subjectDtoList = ConvertEntities
                .fromEntities(subjectEntityPage.toList(), SubjectFactory::fromEntity);
        return new PageImpl<>(subjectDtoList, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                subjectEntityPage.getTotalElements());
    }

    public Page<SubjectDto> findSubjectPage(String objectId, ObjectType objectType, int page, int size) {
        Page<SubjectEntity> subjectEntityPage = subjectRepository.getSubjectPageByObject(objectId, objectType,
                PageRequest.of(page, size));
        List<SubjectDto> subjectDtoList = ConvertEntities
                .fromEntities(subjectEntityPage.toList(), SubjectFactory::fromEntity);
        return new PageImpl<>(subjectDtoList, PageRequest.of(page, size),
                subjectEntityPage.getTotalElements());
    }

    public List<SubjectDto> findSubjectAllowedListByUser() {
        List<SubjectEntity> subjectEntityList = subjectRepository.findAllowedItemList(
                SecurityUtils.getCustomReadPermission(DomainEnum.SUBJECT_DOMAIN_NAME.getValue()));
        return ConvertEntities.fromEntities(subjectEntityList, SubjectFactory::fromEntity);
    }

    public List<SubjectDto> getSubjectByContactPerson(String contactPersonId) {
        List<SubjectEntity> subjectEntityList = subjectRepository.getCustomerEntityListByEntity(contactPersonId);
        return ConvertEntities
                .fromEntities(subjectEntityList, SubjectFactory::fromEntity);
    }

    private Specification<SubjectEntity> getSubjectSpecification(SubjectFilterDto filter) {
        return (Root<SubjectEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getSubjectPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getSubjectPredicateList(
            SubjectFilterDto filter, Root<SubjectEntity> root, CriteriaBuilder criteriaBuilder
    ) {
        List<Predicate> predicates = new ArrayList<>();

        if (!SecurityUtils.hasCustomReadAll(DomainEnum.SUBJECT_DOMAIN_NAME.getValue())) {
            predicates.add(root.get("id").in(SecurityUtils.getCustomReadPermission(
                    DomainEnum.SUBJECT_DOMAIN_NAME.getValue())));
        }

        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.isShowDeleted()));

        if (!StringUtils.isEmpty(filter.getId())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("id")), "%".concat(filter.getId().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getName())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("name")), "%".concat(filter.getName().toLowerCase()).concat("%")));
        }

        if (!StringUtils.isEmpty(filter.getDescription())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("description")),
                    "%".concat(filter.getDescription().toLowerCase()).concat("%")
            ));
        }

        if (!StringUtils.isEmpty(filter.getUrl())) {
            predicates.add(criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("url")),
                    "%".concat(filter.getUrl().toLowerCase()).concat("%")
            ));
        }

        if (CollectionUtils.isNotEmpty(filter.getSubjectTypeSet())) {
            List<Predicate> predicateList = new ArrayList<>();
            if (filter.getSubjectTypeSet().contains(SubjectType.CUSTOMER)) {
                predicateList.add(criteriaBuilder.equal(root.get("customer"), true));
            }
            if (filter.getSubjectTypeSet().contains(SubjectType.SUPPLIER)) {
                predicateList.add(criteriaBuilder.equal(root.get("supplier"), true));
            }
            if (filter.getSubjectTypeSet().contains(SubjectType.OWN_COMPANY)) {
                predicateList.add(criteriaBuilder.equal(root.get("ownCompany"), true));
            }
            predicates.add(criteriaBuilder.or(predicateList.toArray(new Predicate[predicateList.size()])));
        }

        if (CollectionUtils.isNotEmpty(filter.getUserDtoSet())) {
            predicates.add(root.get("userEntity").get("id").in(filter.getUserDtoSet().stream()
                    .map(UserDto::getId)
                    .toList()));
        }

        return predicates;
    }
}
