package cz.bbn.cerberus.user;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.config.SpringContext;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.role.persistance.repository.RoleRepository;
import cz.bbn.cerberus.translation.enums.ApplicationTranslation;
import cz.bbn.cerberus.user.dto.UserActiveRoleDto;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.user.dto.UserFilterDto;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserActiveRoleEntity;
import cz.bbn.cerberus.user.persistance.UserActiveRoleId;
import cz.bbn.cerberus.user.persistance.UserActiveRoleRepository;
import cz.bbn.cerberus.user.persistance.UserDao;
import cz.bbn.cerberus.user.persistance.UserEntity;
import cz.bbn.cerberus.user.persistance.UserRepository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.SerializationUtils;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;


@Service
@Slf4j
public class UserService{

    private final UserRepository userRepository;
    private final UserDao userDao;
    private final AppLogService appLogService;
    private final UserActiveRoleRepository userActiveRoleRepository;
    private final UserValues userValues;
    private final ListService listService;
    private final RoleRepository roleRepository;

    public UserService(UserRepository userRepository, UserDao userDao,
                       AppLogService appLogService, UserActiveRoleRepository userActiveRoleRepository,
                       UserValues userValues, ListService listService, RoleRepository roleRepository) {
        this.userRepository = userRepository;
        this.userDao = userDao;
        this.appLogService = appLogService;
        this.userActiveRoleRepository = userActiveRoleRepository;
        this.userValues = userValues;
        this.listService = listService;
        this.roleRepository = roleRepository;
    }

    @Transactional
    public void setApplicationTranslation(ApplicationTranslation applicationTranslation) {
        UserValues springBean = SpringContext.getBean(UserValues.class);
        springBean.setApplicationTranslation(applicationTranslation);
        userRepository.updateLanguage(applicationTranslation.name().toLowerCase(), SecurityUtils.getCurrentUserId());
    }

    public static ApplicationTranslation getApplicationTranslation() {
        UserValues springBean = SpringContext.getBean(UserValues.class);
        return springBean.getApplicationTranslation();
    }

    public UserDto getUser(Long id) throws SystemException {
        return UserFactory.fromEntity(getUserEntity(id));
    }

    public Set<Long> findUserIdListByBoRole() {
        List<UserEntity> userDtoList = userRepository.findAllAllowed();
        Set<Long> backOfficeIdList = new HashSet<>();
        userDtoList.forEach(userEntity -> userEntity.getUserActiveRoleEntitySet().forEach(userActiveRoleEntity -> {
            if (Boolean.TRUE.equals(userActiveRoleEntity.getRoleEntity().getBackOffice())) {
                backOfficeIdList.add(userEntity.getId());
            }
        }));
        return backOfficeIdList;
    }

    public Set<Long> findUserIdListByInfRole() {
        Set<Long> userIdSet = new HashSet<>();
        List<UserEntity> userDtoList = userRepository.findAllAllowed();
        List<String> infIdList = roleRepository.findInfrastructureRoleId();
        if (infIdList != null && !infIdList.isEmpty()) {
            for (String roleId : infIdList) {
                for (UserEntity userEntity : userDtoList) {
                    if (userEntity.getUserRoles() != null && userEntity.getUserRoles().contains(roleId)) {
                        userIdSet.add(userEntity.getId());
                    }
                }
            }
        }
        return userIdSet;
    }

    public Page<UserDto> findUserPageDto(UserFilterDto userFilterDto) {
        return userDao.findUserPage(userFilterDto);
    }

    public List<UserDto> findUserList() {
        return ConvertEntities
                .fromEntities(userRepository.findAll(), UserFactory::fromEntity);
    }

    @Transactional
    public UserDto findUser(String login, String name, String azureId) {
        UserEntity userEntity = userRepository.findByLogin(login);
        UserDto userDto;
        if (userEntity == null) {
            userEntity = new UserEntity();
            userEntity.setDeleted(false);
            userEntity.setLogin(login);
            userEntity.setName(name);
            userEntity.setMail(login);
            if (userEntity.getSendUnreadMails() == null) {
                userEntity.setSendUnreadMails(false);
            }
            userEntity = userRepository.save(userEntity);
            userDto = UserFactory.fromEntity(userEntity);
            appLogService.log("User insert", userEntity.getName(), userEntity.getId());
            listService.reloadUserDtoList();
        } else {
            userDto = UserFactory.fromEntity(userEntity);
        }
        if (azureId != null && userEntity.getAzureId() == null) {
            userEntity.setAzureId(azureId);
            userRepository.save(userEntity);
            userDto.setAzureId(azureId);
        }
        Set<UserActiveRoleDto> userActiveRoleDtoSet = new HashSet<>();
        if (userEntity.getUserActiveRoleEntitySet() != null) {
            userEntity.getUserActiveRoleEntitySet().forEach(userActiveRoleEntity -> {
                UserActiveRoleDto userActiveRoleDto = new UserActiveRoleDto(userActiveRoleEntity.getId().getRoleId());
                userActiveRoleDtoSet.add(userActiveRoleDto);
            });
        }
        userDto.setUserActiveRoleDtoSet(userActiveRoleDtoSet);
        String language = Optional.ofNullable(userDto.getPreferredLanguage()).orElse("cs");
        userValues.setApplicationTranslation(ApplicationTranslation.valueOf(language.toUpperCase()));
        return userDto;
    }

    @Transactional
    public void updateUser(UserDto dto, UserDto originalDto) throws SystemException {
        List<UserDto> userList = findUserList();
        for (UserDto user : userList) {
            if (user.getEmployee() != null && !user.getId().equals(dto.getId())
                    && Objects.equals(user.getEmployee(), dto.getEmployee())) {
                throw new SystemException(ErrorCode.EMPLOYEE_ALREADY_ASSIGNED);
            }
        }
        UserEntity userEntity = getUserEntity(dto.getId());
        UserFactory.fillEntity(userEntity, dto);
        userRepository.save(userEntity);
        appLogService.logUpdate(dto, originalDto, DomainEnum.USER_DOMAIN_NAME.getValue());
        listService.reloadUserDtoList();
    }

    @Transactional
    public void deleteUser(Long id) throws SystemException {
        UserEntity userEntity = getUserEntity(id);
        userEntity.setDeleted(true);
        userRepository.save(userEntity);
        listService.reloadUserDtoList();
    }

    @Transactional
    public void updateActiveUserRole(Long userId, Set<String> activeRoleSet) {
        userActiveRoleRepository.deleteByIdUserId(userId);
        activeRoleSet.forEach(roleId -> {
            UserActiveRoleEntity userActiveRoleEntity = new UserActiveRoleEntity();
            userActiveRoleEntity.setId(new UserActiveRoleId(userId, roleId));
            userActiveRoleRepository.save(userActiveRoleEntity);
        });
    }

    private UserEntity getUserEntity(Long id) throws SystemException {
        return userRepository.findById(id).orElseThrow(() -> new SystemException(ErrorCode.USER_NOT_EXISTS, id));
    }

    public List<UserDto> findAllowedUserList() {
        return ConvertEntities.fromEntities(userRepository.findAllAllowed(), UserFactory::fromEntity);
    }

    public Map<Long, String> getOwnerMap() {
        List<UserDto> userList = findUserList();
        Map<Long, String> userMap = new HashMap<>();
        for (UserDto user : userList) {
            if (user.getAcronym() != null && !user.getAcronym().trim().isEmpty()) {
                userMap.put(user.getId(), user.getAcronym());
            } else {
                userMap.put(user.getId(), user.getName());
            }
        }
        return userMap;
    }

    public void saveRoles(UserDto dto) {
        try {
            UserEntity userEntity = getUserEntity(dto.getId());
            UserDto originalDto = UserFactory.fromEntity(userEntity);
            UserDto userDto = SerializationUtils.clone(originalDto);
            userDto.setUserRoles(dto.getUserRoles());
            userEntity.setUserRoles(dto.getUserRoles());
            userRepository.save(userEntity);
            appLogService.log("Role update", userDto.getUserRoles(), dto.getId());
            listService.reloadUserDtoList();
        } catch (SystemException e) {
            log.error(e.getMessage(), e);
        }
    }
}
