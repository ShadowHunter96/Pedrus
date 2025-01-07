package cz.bbn.cerberus.config;

import com.nimbusds.jose.shaded.json.JSONArray;
import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.AppUser;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.role.RoleService;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.UserValues;
import cz.bbn.cerberus.user.dto.UserActiveRoleDto;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.user.factory.UserFactory;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.oauth2.client.authentication.OAuth2AuthenticationToken;
import org.springframework.security.oauth2.client.oidc.userinfo.OidcUserRequest;
import org.springframework.security.oauth2.client.oidc.userinfo.OidcUserService;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserService;
import org.springframework.security.oauth2.core.oidc.user.OidcUser;
import org.springframework.web.context.annotation.SessionScope;

import javax.annotation.PostConstruct;
import java.util.HashSet;
import java.util.Set;

@Configuration
@Slf4j
public class OAuth2AuthentificationConfig {

    private final RoleService roleService;
    private final AppLogService appLogService;
    private final UserService userService;
    private final CustomPermissionService customPermissionService;
    private final UserValues userValues;

    public OAuth2AuthentificationConfig(RoleService roleService, AppLogService appLogService,
                                        UserService userService, CustomPermissionService customPermissionService,
                                        UserValues userValues) {
        this.roleService = roleService;
        this.appLogService = appLogService;
        this.userService = userService;
        this.customPermissionService = customPermissionService;
        this.userValues = userValues;
    }

    @Bean
    public OAuth2UserService<OidcUserRequest, OidcUser> oidcUserService() {
        final OidcUserService delegate = new OidcUserService();
        return userRequest -> {
            // Delegate to the default implementation for loading a user
            OidcUser oidcUser = delegate.loadUser(userRequest);
            appLogService.log("login", "azure info: ".concat(oidcUser.getAttributes().toString()), "", 0L);
            JSONArray jsonElements = oidcUser.getClaim("roles");
            Set<RoleDto> roleDtoSet = new HashSet<>();
            if (jsonElements != null && !jsonElements.isEmpty()) {
                jsonElements.forEach(role -> {
                    try {
                        roleDtoSet.add(roleService.getRole(String.valueOf(role)));
                    } catch (SystemException e) {
                        log.error("System exception", e.getLocalizedMessage());
                    }
                });
            }
            AppUser appUser = new AppUser(new HashSet<>(), oidcUser.getIdToken(), oidcUser.getUserInfo());
            userValues.setAppUser(appUser);

            UserDto userDto = userService.findUser(appUser.getUsername(), appUser.getName(),
                    (String) userRequest.getIdToken().getClaims().getOrDefault("oid", null));
            Set<RoleDto> activeRoleDtoSet = new HashSet<>();
            roleDtoSet.forEach(roleDto -> {
                if (userDto.getUserActiveRoleDtoSet().contains(new UserActiveRoleDto(roleDto.getId()))) {
                    activeRoleDtoSet.add(roleDto);
                }
            });
            appUser.setByUserDto(userDto, roleDtoSet, activeRoleDtoSet.isEmpty() ? roleDtoSet : activeRoleDtoSet);
            appUser.setAzureBearerToken(userRequest.getAccessToken().getTokenValue());
            customPermissionService.loadPermissionSetByCurrentUser();
            userService.saveRoles(UserFactory.fromUserDto(appUser));
            return appUser;
        };
    }
}
