package cz.bbn.cerberus.user.persistance;

import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.user.dto.UserFilterDto;
import cz.bbn.cerberus.user.factory.UserFactory;
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
public class UserDao {

    private final UserRepository userRepository;

    public UserDao(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    public Page<UserDto> findUserPage(UserFilterDto userFilterDto) {
        Page<UserEntity> userEntityPage = userRepository.findAll(getUserSpecification(userFilterDto),
                PageRequest.of(userFilterDto.getPage(), userFilterDto.getSize(),
                        Sort.by(userFilterDto.getOrderList())));
        List<UserDto> supplierDtoList = ConvertEntities
                .fromEntities(userEntityPage.toList(), UserFactory::fromEntity);
        return new PageImpl<>(supplierDtoList, PageRequest.of(userFilterDto.getPage(),
                userFilterDto.getSize(), Sort.by(userFilterDto.getOrderList())),
                userEntityPage.getTotalElements());
    }

    private Specification<UserEntity> getUserSpecification(UserFilterDto filter) {
        return (Root<UserEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.and(getUserPredicateList(filter, root, criteriaBuilder).toArray(new Predicate[0]));
    }

    private List<Predicate> getUserPredicateList(
            UserFilterDto filter, Root<UserEntity> root, CriteriaBuilder criteriaBuilder
    ) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(criteriaBuilder.equal(root.get("deleted"), filter.isShowDeleted()));

        if (!StringUtils.isEmpty(filter.getName())) {
            predicates.add(criteriaBuilder.like(root.get("login"), "%".concat(filter.getName()).concat("%")));
        }

        return predicates;
    }
}
