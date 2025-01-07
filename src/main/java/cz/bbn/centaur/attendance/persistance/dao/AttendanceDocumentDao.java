package cz.bbn.cerberus.attendance.persistance.dao;

import cz.bbn.cerberus.attendance.dto.AttendanceSimpleDocumentDto;
import cz.bbn.cerberus.attendance.dto.AttendanceDocumentFilterDto;
import cz.bbn.cerberus.attendance.factory.AttendanceFactory;
import cz.bbn.cerberus.attendance.persistance.entity.AttendanceSimpleDocumentEntity;
import cz.bbn.cerberus.attendance.persistance.repository.AttendanceSimpleDocumentRepository;
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
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Component
public class AttendanceDocumentDao {

    private final AttendanceSimpleDocumentRepository attendanceSimpleDocumentRepository;

    public AttendanceDocumentDao(AttendanceSimpleDocumentRepository attendanceSimpleDocumentRepository) {
        this.attendanceSimpleDocumentRepository = attendanceSimpleDocumentRepository;
    }

    public Page<AttendanceSimpleDocumentDto> findAttendanceDocumentDtoPage(AttendanceDocumentFilterDto filter) {
        Page<AttendanceSimpleDocumentEntity> page = attendanceSimpleDocumentRepository.findAll(getApprovementSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(filter.getOrderList())));
        List<AttendanceSimpleDocumentDto> list = ConvertEntities
                .fromEntities(page.toList(), AttendanceFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(filter.getOrderList())),
                page.getTotalElements());
    }

    private Specification<AttendanceSimpleDocumentEntity> getApprovementSpecification(AttendanceDocumentFilterDto filter) {
        return (Root<AttendanceSimpleDocumentEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getApprovementList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getApprovementList(AttendanceDocumentFilterDto filter,
                                               Root<AttendanceSimpleDocumentEntity> root,
                                               CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.isShowDeleted()));

        if (filter.getFrom() != null) {
            predicates.add(criteriaBuilder.greaterThanOrEqualTo(
                    root.get("date"), LocalDateTime.from(filter.getFrom())));
        }

        if (filter.getTo() != null) {
            predicates.add(criteriaBuilder.lessThanOrEqualTo(
                    root.get("date"), LocalDateTime.from(filter.getFrom())));
        }

        if (filter.getUserDto() != null) {
            predicates.add(criteriaBuilder.equal(root.get("userEntity").get("id"), filter.getUserDto().getId()));
        }

        return predicates;
    }
}
