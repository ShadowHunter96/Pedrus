package cz.bbn.cerberus.commons.security;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.enviromennt.AppProperty;
import cz.bbn.cerberus.role.RoleService;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.UserValues;
import cz.bbn.cerberus.user.dto.UserActiveRoleDto;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.user.factory.UserFactory;
import lombok.SneakyThrows;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;

import java.util.HashSet;
import java.util.Set;


@Component
@ConditionalOnExpression("'${project.ldap.login.enabled}' == 'false' && '${spring.cloud.azure.active-directory.enabled}' == 'false'")
public class LocalAuthenticationProvider implements AuthenticationProvider {

    private final AppLogService appLogService;
    private final RoleService roleService;
    private final AppEnv appEnv;
    private final UserService userService;
    private final UserValues userValues;

    public LocalAuthenticationProvider(AppLogService appLogService, RoleService roleService,
                                       AppEnv appEnv, UserService userService, UserValues userValues) {
        this.appLogService = appLogService;
        this.roleService = roleService;
        this.appEnv = appEnv;
        this.userService = userService;
        this.userValues = userValues;
    }

    @SneakyThrows
    @Override
    public Authentication authenticate(Authentication auth) {
        Set<RoleDto> roleDtoSet = new HashSet<>();
        roleDtoSet.add(roleService.getRole(appEnv.getProperty(AppProperty.LOCAL_LOGIN_ROLE)));
        String userName = appEnv.getProperty(AppProperty.LOCAL_LOGIN_USER_NAME);
        String mail = appEnv.getStringProperty(AppProperty.LOCAL_LOGIN_MAIL);
        AppUser appUser = new AppUser(mail, userName, mail);

        UserDto userDto = userService.findUser(mail, userName, null);
        Set<RoleDto> activeRoleDtoSet = new HashSet<>();
        roleDtoSet.forEach(roleDto -> {
            if (userDto.getUserActiveRoleDtoSet().contains(new UserActiveRoleDto(roleDto.getId()))) {
                activeRoleDtoSet.add(roleDto);
            }
        });
        appUser.setByUserDto(userDto, roleDtoSet, activeRoleDtoSet.isEmpty() ? roleDtoSet : activeRoleDtoSet);
        appLogService.logLogin(appUser.getId(),
                "role: ".concat(AppProperty.LOCAL_LOGIN_ROLE.toString()));
        userService.saveRoles(UserFactory.fromUserDto(appUser));
        userValues.setAppUser(appUser);
        return new UsernamePasswordAuthenticationToken(appUser, "", appUser.getAuthorities());
    }

    @Override
    public boolean supports(Class<?> auth) {
        return auth.equals(UsernamePasswordAuthenticationToken.class);
    }

}
