package cz.bbn.cerberus.permissionmanagement;

import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.role.RoleService;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.dto.UserDto;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Component
public class PermissionManagementComponentOperations {

    private final UserService userService;
    private final RoleService roleService;

    public PermissionManagementComponentOperations(UserService userService, RoleService roleService) {
        this.userService = userService;
        this.roleService = roleService;
    }

    public Map<String, List<String>> getObjectMap() {
        return SecurityUtils.getObjectMap();
    }

    public List<UserDto> getUserList() {
        return userService.findUserList();
    }

    public List<RoleDto> getRoleList() {
        return roleService.findAll();
    }
}
