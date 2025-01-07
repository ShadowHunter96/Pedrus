package cz.bbn.cerberus.role;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.role.dto.RoleHasPermissionDto;
import cz.bbn.cerberus.role.factory.RoleFactory;
import cz.bbn.cerberus.role.factory.RoleHasPermissionFactory;
import cz.bbn.cerberus.role.persistance.entity.RoleEntity;
import cz.bbn.cerberus.role.persistance.entity.RoleHasPermissionEntity;
import cz.bbn.cerberus.role.persistance.repository.RoleHasPermissionRepository;
import cz.bbn.cerberus.role.persistance.repository.RoleRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import javax.persistence.EntityNotFoundException;
import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;


@Service
@Slf4j
public class RoleService {

    private final RoleRepository roleRepository;
    private final RoleHasPermissionRepository roleHasPermissionRepository;
    private final AppLogService appLogService;
    private final AppEnv appEnv;

    public RoleService(RoleRepository roleRepository, RoleHasPermissionRepository roleHasPermissionRepository,
                       AppLogService appLogService, AppEnv appEnv) {
        this.roleRepository = roleRepository;
        this.roleHasPermissionRepository = roleHasPermissionRepository;
        this.appLogService = appLogService;
        this.appEnv = appEnv;
    }

    public Page<RoleEntity> findAll(int page, int pageSize, List<Sort.Order> orderList) {
        return roleRepository.findAll(PageRequest.of(page, pageSize, Sort.by(orderList)));
    }

    public List<RoleDto> findAll() {
        Map<String, Set<RoleHasPermissionDto>> roleHasPermMap = new HashMap<>();
        List<RoleDto> roleDtoList = ConvertEntities.fromEntities(roleRepository.findAll(), RoleFactory::fromEntity);
        List<RoleHasPermissionDto> roleHasPermissionDtoSet = ConvertEntities.fromEntities(
                roleHasPermissionRepository.findAll(), RoleHasPermissionFactory::fromEntity);
        for (RoleHasPermissionDto roleHasPermissionDto : roleHasPermissionDtoSet) {
            Set<RoleHasPermissionDto> dtoSet;
            if (roleHasPermMap.containsKey(roleHasPermissionDto.getRoleId())) {
                dtoSet = roleHasPermMap.get(roleHasPermissionDto.getRoleId());
            } else {
                dtoSet = new HashSet<>();
            }
            dtoSet.add(roleHasPermissionDto);
            roleHasPermMap.put(roleHasPermissionDto.getRoleId(), dtoSet);
        }

        for (RoleDto roleDto : roleDtoList) {
            if (roleHasPermMap.containsKey(roleDto.getId())) {
                roleDto.setRoleHasPermissionSet(roleHasPermMap.get(roleDto.getId()));
            }
        }

        return roleDtoList;
    }

    public RoleDto getRole(String code) throws SystemException {
        RoleEntity roleEntity = getRoleEntity(code);
        RoleDto roleDto = RoleFactory.fromEntity(roleEntity);
        roleDto.setRoleHasPermissionSet(ConvertEntities.fromEntities(roleHasPermissionRepository.getByRoleId(code),
                RoleHasPermissionFactory::fromEntity));
        return roleDto;
    }

    public boolean roleExists(String id) {
        return roleRepository.existsById(id);
    }

    @Transactional
    public void saveRole(RoleDto roleDto) throws SystemException {
        if (roleRepository.existsById(roleDto.getId())) {
            throw new SystemException(ErrorCode.ROLE_ALREADY_EXISTS, roleDto.getId());
        }
        checkInfrastructure(roleDto);
        RoleEntity roleEntity = new RoleEntity();
        saveRole(roleEntity, roleDto);
        appLogService.logInsert(roleDto, DomainEnum.ROLE_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void updateRole(RoleDto roleDto, RoleDto originalDto) throws SystemException {
        RoleEntity roleEntity = getRoleEntity(roleDto.getId());
        checkInfrastructure(roleDto);
        saveRole(roleEntity, roleDto);
        appLogService.logUpdate(roleDto, originalDto, DomainEnum.ROLE_DOMAIN_NAME.getValue());
    }

    private void checkInfrastructure(RoleDto roleDto) {
        if (Boolean.TRUE.equals(roleDto.getInfrastructure())) {
            List<RoleEntity> roleList = roleRepository.getByInf();
            List<RoleEntity> changedRolelist = new ArrayList<>();
            for (RoleEntity roleEntity : roleList) {
                roleEntity.setInfrastructure(false);
                changedRolelist.add(roleEntity);
            }
            if (!changedRolelist.isEmpty()) {
                roleRepository.saveAll(changedRolelist);
            }
        }
    }

    @Transactional
    public void deleteRole(String id) throws SystemException {
        RoleEntity roleEntity = getRoleEntity(id);
        roleRepository.deleteById(roleEntity.getId());
        appLogService.logDelete(id, DomainEnum.ROLE_DOMAIN_NAME.getValue());
    }

    public void reloadRoleSet(Set<String> roleSet) {
        Set<RoleDto> roleDtoSet = new HashSet<>();
        roleSet.forEach(role -> {
            try {
                roleDtoSet.add(getRole(String.valueOf(role)));
            } catch (SystemException ex) {
                log.error(ex.getMessage());
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        });
        Set<String> permissionList = new HashSet<>();

        roleDtoSet.forEach(roleDto ->
                permissionList.addAll(getRolePermissionSet(roleDto.getId())));
        SecurityUtils.updateAuthorities(permissionList, appEnv, roleDtoSet);
    }

    private Set<String> getRolePermissionSet(String id) {
        return roleHasPermissionRepository.getPermissionByRoleId(id);
    }

    private void fillPermissions(String roleId, Set<RoleHasPermissionDto> roleHasPermissionDtos) {
        roleHasPermissionRepository.deleteByRoleId(roleId);
        Set<RoleHasPermissionEntity> roleHasPermissionEntities = new HashSet<>();
        roleHasPermissionDtos.forEach(item -> {
            RoleHasPermissionEntity roleHasPermissionEntity =
                    RoleHasPermissionFactory.fillRoleHasPermission(roleId, item);
            roleHasPermissionEntities.add(roleHasPermissionEntity);
        });
        roleHasPermissionRepository.saveAll(roleHasPermissionEntities);
    }

    @Transactional
    public void savePermissions(Map<String, Set<RoleHasPermissionDto>> rolePermMap) {
        roleHasPermissionRepository.deleteAll();
        Set<RoleHasPermissionEntity> allPerm = new HashSet<>();
        for (String roleId : rolePermMap.keySet()) {
            rolePermMap.get(roleId).forEach((RoleHasPermissionDto item) -> {
                RoleHasPermissionEntity roleHasPermissionEntity =
                        RoleHasPermissionFactory.fillRoleHasPermission(roleId, item);
                allPerm.add(roleHasPermissionEntity);
            });
        }
        roleHasPermissionRepository.saveAll(allPerm);
    }

    public RoleDto getByBackOffice() throws SystemException {
        RoleEntity roleEntity = roleRepository.findByBackOffice(true).orElseThrow(() ->
                new SystemException(ErrorCode.UNSET_BACK_OFFICE));
        return RoleFactory.fromEntity(roleEntity);
    }

    private void saveRole(RoleEntity roleEntity, RoleDto roleDto) {
        RoleFactory.fillEntity(roleEntity, roleDto);
        if (roleDto.getBackOffice()) {
            roleRepository.updateBackOffice(roleDto.getId());
        }
        RoleEntity actualRoleEntity = roleRepository.save(roleEntity);
        fillPermissions(actualRoleEntity.getId(), roleDto.getRoleHasPermissionSet());
    }

    private RoleEntity getRoleEntity(String code) throws SystemException {
        if ("".equals(code)) {
            RoleEntity role = new RoleEntity();
            return role;
        }
        try {
            return roleRepository.findByIdIgnoreCase(code).orElseThrow(() ->
                    new SystemException(ErrorCode.ROLE_NOT_EXISTS, code));
        } catch (EntityNotFoundException e) {
            throw new SystemException(ErrorCode.ROLE_NOT_EXISTS, code);
        }
    }

    public Set<String> findAllId() {
        return roleRepository.findAllId();
    }

    public RoleDto getInfrastructureRole() throws SystemException {
        RoleEntity roleEntity = roleRepository.findByInfrastructure(true).orElseThrow(() ->
                new SystemException(ErrorCode.UNSET_INFRASTRUCTURE));
        return RoleFactory.fromEntity(roleEntity);
    }
}
