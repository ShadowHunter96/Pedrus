package cz.bbn.cerberus.usermessage.persistance;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.usermessage.UserMessageObjectType;
import cz.bbn.cerberus.usermessage.dto.UserMessageDto;
import cz.bbn.cerberus.usermessage.dto.UserMessageFilterDto;
import cz.bbn.cerberus.usermessage.factory.UserMessageFactory;
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
public class UserMessageDao {

    private final UserMessageRepository userMessageRepository;

    public UserMessageDao(UserMessageRepository userMessageRepository) {
        this.userMessageRepository = userMessageRepository;
    }

    public Page<UserMessageDto> findUserMessageDtoPage(UserMessageFilterDto filter) {
        List<Sort.Order> orderList = new ArrayList<>();
        orderList.add(Sort.Order.asc("viewed"));
        orderList.add(Sort.Order.desc("id"));
        if (filter.isOrderByPriority()) {
            orderList.add(Sort.Order.desc("priority"));
        }
        orderList.add(Sort.Order.desc("dueDate"));

        Page<UserMessageEntity> page = userMessageRepository.findAll(getUserMessagePageSpecification(filter),
                PageRequest.of(filter.getPage(), filter.getSize(), Sort.by(orderList)));
        List<UserMessageDto> list = ConvertEntities
                .fromEntities(page.toList(), UserMessageFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(filter.getPage(),
                filter.getSize(), Sort.by(orderList)),
                page.getTotalElements());
    }

    public UserMessageDto findLastUnviewed(UserMessageFilterDto filterDto) {
        List<Sort.Order> orderList = new ArrayList<>();
        orderList.add(Sort.Order.asc("id"));
        if (filterDto.isOrderByPriority()) {
            orderList.add(Sort.Order.desc("priority"));
        }
        orderList.add(Sort.Order.desc("dueDate"));
        Page<UserMessageEntity> userMessageEntityList = userMessageRepository.findAll(
                getUserMessageSpecification(filterDto), PageRequest.of(0, 1, Sort.by(orderList)));
        if (userMessageEntityList.isEmpty()) {
            return null;
        }
        return UserMessageFactory.fromEntity(userMessageEntityList.getContent().get(0));
    }

    private Specification<UserMessageEntity> getUserMessageSpecification(UserMessageFilterDto filter) {
        return (Root<UserMessageEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getUserMessagePredicate(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getUserMessagePredicate(UserMessageFilterDto filter, Root<UserMessageEntity> root,
                                                    CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(criteriaBuilder.equal(root.get("userId"), filter.getUserId()));
        predicates.add(criteriaBuilder.equal(root.get("viewed"), Boolean.FALSE));

        return predicates;
    }

    private Specification<UserMessageEntity> getUserMessagePageSpecification(UserMessageFilterDto filter) {
        return (Root<UserMessageEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(
                        getUserMessagePagePredicate(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getUserMessagePagePredicate(UserMessageFilterDto filter, Root<UserMessageEntity> root,
                                                        CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(criteriaBuilder.equal(root.get("userId"), filter.getUserId()));

        if (filter.getObjectType() != UserMessageObjectType.ALL) {
            predicates.add(criteriaBuilder.equal(root.get("objectType"), filter.getObjectType()));
        }

        return predicates;
    }
}
