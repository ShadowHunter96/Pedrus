package cz.bbn.cerberus.commons.security;

import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.role.RoleService;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.UserValues;
import cz.bbn.cerberus.user.dto.UserActiveRoleDto;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.user.factory.UserFactory;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;

import java.util.HashSet;
import java.util.Set;


@Component
@ConditionalOnExpression("'${project.ldap.login.enabled}' == 'true'")
@Slf4j
public class LdapAuthenticationProvider implements AuthenticationProvider {

    private final RoleService roleService;
    private final LdapService ldapService;
    private final UserService userService;
    private final UserValues userValues;

    public LdapAuthenticationProvider(RoleService roleService, LdapService ldapService,
                                      UserService userService, UserValues userValues) {
        this.roleService = roleService;
        this.ldapService = ldapService;
        this.userService = userService;
        this.userValues = userValues;
    }


    @SneakyThrows
    @Override
    public Authentication authenticate(Authentication auth) {
        try {
            boolean authed = ldapService.authenticate(auth);
            if (authed) {
                AppUser appUser = ldapService.getAppUserFromLdap(auth.getName());
                if (appUser.getRoleSet() == null) {
                    throw new SystemException(ErrorCode.BAD_LDAP_ROLE);
                }
                UserDto userDto = userService.findUser(auth.getName(), appUser.getName(), null);
                Set<RoleDto> roleDtoSet = new HashSet<>();
                appUser.getRoleSet().forEach(s -> {
                    try{
                        roleDtoSet.add(roleService.getRole(s));
                    } catch (SystemException e) {
                        log.error("System exception", e);
                    }
                });
                Set<RoleDto> activeRoleDtoSet = new HashSet<>();
                roleDtoSet.forEach(roleDto -> {
                    if(userDto.getUserActiveRoleDtoSet().contains(new UserActiveRoleDto(roleDto.getId()))){
                        activeRoleDtoSet.add(roleDto);
                    }
                });
                appUser.setId(userDto.getId());
                userService.saveRoles(UserFactory.fromUserDto(appUser));
                appUser.setByUserDto(userDto, roleDtoSet, activeRoleDtoSet.isEmpty() ? roleDtoSet : activeRoleDtoSet);
                userValues.setAppUser(appUser);
                return new UsernamePasswordAuthenticationToken(appUser,
                        auth.getCredentials().toString(), appUser.getAuthorities());
            } else {
                return null;
            }
        } catch (SystemException ex) {
            log.error("Login exception", ex);
            throw new SystemException(ErrorCode.LOGIN_ERROR, ex.getErrorCode().getError());
        } catch (Exception ex) {
            log.error("Login exception", ex);
            throw new SystemException(ErrorCode.UNKNOWN_LOGIN_ERROR);
        }

    }

    @Override
    public boolean supports(Class<?> auth) {
        return auth.equals(UsernamePasswordAuthenticationToken.class);
    }

}
