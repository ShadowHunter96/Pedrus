package cz.bbn.cerberus.commons.security;

import com.vaadin.flow.server.HandlerHelper;
import com.vaadin.flow.shared.ApplicationConstants;
import cz.bbn.cerberus.commons.AppUrlValues;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.config.SpringContext;
import cz.bbn.cerberus.custompermission.dto.CustomPermType;
import cz.bbn.cerberus.custompermission.dto.PermUserDto;
import cz.bbn.cerberus.mainlayout.ui.MenuItem;
import cz.bbn.cerberus.mainlayout.ui.SubMenuItem;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.user.UserValues;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.user.factory.UserFactory;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.oauth2.client.authentication.OAuth2AuthenticationToken;

import javax.servlet.http.HttpServletRequest;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Stream;

/**
 * utils class containing methods for permission evaluation
 */
@Slf4j
public final class SecurityUtils {

    private SecurityUtils() {
        // Util methods only
    }

    public static boolean isFrameworkInternalRequest(HttpServletRequest request) {
        final String parameterValue = request.getParameter(ApplicationConstants.REQUEST_TYPE_PARAMETER);
        return parameterValue != null
                && Stream.of(HandlerHelper.RequestType.values()).anyMatch(
                r -> r.getIdentifier().equals(parameterValue));
    }

    public static UserDto appUserToUserDto() {
        AppUser appUser = getCurrentUser();
        UserDto userDto = new UserDto();
        userDto.setLogin(appUser.getBid());
        userDto.setName(appUser.getName());
        userDto.setId(appUser.getId());
        return userDto;
    }

    public static String getCurrentUserName() {
        AppUser appUser = getCurrentUser();
        if (appUser == null) {
            return null;
        }
        return appUser.getName();
    }

    public static Long getCurrentUserId() {
        try {
            return Objects.requireNonNull(getCurrentUser()).getId();
        } catch (NullPointerException e) {
            return -1L;
        }
    }

    public static String getLoginName() {
        AppUser appUser = getCurrentUser();
        if (appUser == null) {
            return null;
        }
        return appUser.getUsername();
    }

    public static UserDto getCurrentUserDto() {
        return UserFactory.fromUserDto(getCurrentUser());
    }

    public static boolean userIsAzureInstance() {
        UserValues userValues = SpringContext.getBean(UserValues.class);
        return userValues.getAppUser().isAzureLogin();
    }

    public static AppUser getCurrentUser() {
        UserValues userValues = SpringContext.getBean(UserValues.class);
        if (userValues == null) {
            return null;
        }
        return userValues.getAppUser();
    }

    public static void updateAuthorities(Set<String> permissionList, AppEnv appEnv, Set<RoleDto> visibleRoleDtoSet) {
        Set<Permission> newAuthoritySet = new HashSet<>();
        Set<String> visibleRoleSet = new HashSet<>();
        visibleRoleDtoSet.forEach(roleDto -> visibleRoleSet.add(roleDto.getId()));

        if (SecurityUtils.getCurrentUser() != null) {
            SecurityUtils.getCurrentUser().setActiveRoleSet(visibleRoleSet);
        }
        for (String permission : permissionList) {
            newAuthoritySet.add(Permission.valueOfOrNotExists(permission));
        }

        UserValues userValues = SpringContext.getBean(UserValues.class);
        AppUser appUser = userValues.getAppUser();
        appUser.setPermissionSet(newAuthoritySet);
        userValues.setAppUser(appUser);

    }

    public static boolean isAccessGranted(MenuItem menuItem) {
        if (menuItem.getSubMenuItems().length < 1) {
            return isAccessGranted(menuItem.getView());
        }
        for (SubMenuItem subMenuItem : menuItem.getSubMenuItems()) {
            if (isAccessGranted(subMenuItem.getView())) {
                return true;
            }
        }
        return false;
    }

    public static boolean isAccessGranted(Class<?> securedClass) {
        Authorize authorize = AnnotationUtils.findAnnotation(securedClass, Authorize.class);
        if (authorize == null) {
            return true;
        }

        UserValues userValues = SpringContext.getBean(UserValues.class);
        List<Permission> allowedRoles = Arrays.asList(authorize.value());

        if (userValues.getAppUser() == null) {
            return false;
        }

        for (GrantedAuthority ga : userValues.getAppUser().getAuthorities()) {
            if (allowedRoles.contains(ga)) {
                return true;
            }
        }
        return false;
    }

    public static boolean hasOneOfPermissions(Permission... permissions) {
        try {
            UserValues userValues = SpringContext.getBean(UserValues.class);
            Collection<? extends GrantedAuthority> authorities = userValues.getAppUser().getAuthorities();

            for (Permission perm : permissions) {
                if (authorities.contains(perm)) {
                    return true;
                }
            }
        } catch (NullPointerException e) {
            log.error("Cannot get user context", e);
        }
        return false;
    }


    public static void securityConfigure(WebSecurity web) {
        web.ignoring().antMatchers("/cerberus-public/**",
                // Vaadin Flow static resources
                "/ui/VAADIN/**", "/VAADIN/**",

                // the robots exclusion standard
                "/robots.txt",

                // web application manifest
                "/manifest.webmanifest", "/sw.js", "/offline-page.html", "/ui/manifest.webmanifest", "/ui/sw.js",

                "/ui/offline-page.html",

                // icons and images
                "/icons/**", "/images/**", "/ui/icons/**", "/ui/images/**", "/icons/**", "/vaadinServlet/**",
                "/cas/**", "/img/**", "/image-png/**",

                // (development mode) static resources
                "/ui/frontend/**", "/frontend/**",

                // (development mode) webjars
                "/ui/webjars/**", "/webjars/**",

                // (production mode) static resources
                "/ui/frontend-es5/**", "/ui/frontend-es6/**", "/frontend-es5/**", "/frontend-es6/**",

                "/error",
                AppUrlValues.ACTUATOR);
    }

    public static Map<String, List<String>> getObjectMap() {
        return CustomPermissionHandler.getInstance().getObjectMap();
    }

    public static void addToCustomPermissionHandler(CustomPermissionProvider service) {
        CustomPermissionHandler.getInstance().addToCustomPermissionHandler(service);
    }

    public static boolean hasPermission(Permission permission) {
        return hasOneOfPermissions(permission);
    }

    public static Set<String> getCustomReadPermission(String objectName) {
        return CustomPermissionHandler.getInstance().getReadPermission(objectName);
    }

    public static boolean hasCustomUserPermission(String objectName, String objectId, String permissionId,
                                                  Long userId) {
        return CustomPermissionHandler.getInstance().hasUserPermission(objectName, objectId, permissionId, userId);
    }

    public static boolean hasCustomPermission(String objectName, String objectId, String permissionId) {
        return CustomPermissionHandler.getInstance().hasPermission(objectName, objectId, permissionId, null);
    }

    public static boolean hasCustomPermission(String objectName, String objectId, String permissionId,
                                              String secondaryId) {
        return CustomPermissionHandler.getInstance().hasPermission(objectName, objectId, permissionId, secondaryId);
    }

    public static Set<String> getCustomPermissionByObjectName(String key) {
        return CustomPermissionHandler.getInstance().getPermissionByObjectName(key);
    }

    public static Map<Long, Map<String, Map<String, Set<String>>>> getUserDomainEntityPermissionMap() {
        return CustomPermissionHandler.getInstance().getUserDomainEntityPermissionMap();
    }

    public static void setUserDomainEntityPermissionMap(Map<Long, Map<String, Map<String, Set<String>>>> map) {
        CustomPermissionHandler.getInstance().setUserDomainEntityPermissionMap(map);
    }

    public static Map<String, Map<String, Map<String, Set<String>>>> getRoleDomainEntityPermissionMap() {
        return CustomPermissionHandler.getInstance().getRoleDomainEntityPermissionMap();
    }

    public static void setRoleDomainEntityPermissionMap(Map<String, Map<String, Map<String, Set<String>>>> map) {
        CustomPermissionHandler.getInstance().setRoleDomainEntityPermissionMap(map);
    }

    public static Set<String> getAllowedEntityIdSet(Set<String> permission) {
        return CustomPermissionHandler.getInstance().getAllowedEntityIdSet(permission);
    }

    public static Set<String> getAllowedEntityIdByDomain(Set<String> permission, String domain) {
        AppUser user = SecurityUtils.getCurrentUser();
        return CustomPermissionHandler.getInstance().getAllowedEntityIdByDomain(permission, domain, user);
    }

    public static Set<String> getAllowedEntityIdByDomain(String permission, String domain) {
        AppUser user = SecurityUtils.getCurrentUser();
        return getAllowedEntityIdByDomain(permission, domain, user);
    }

    public static Set<String> getAllowedEntityIdByDomain(String permission, String domain, AppUser user) {
        Set<String> permSet = new HashSet<>();
        permSet.add(permission);
        return CustomPermissionHandler.getInstance().getAllowedEntityIdByDomain(permSet, domain, user);
    }

    public static boolean hasCustomReadAll(String domain) {
        return CustomPermissionHandler.getInstance().hasCustomReadAll(domain);
    }

    public static Set<String> getSecondaryOwnerSet(String domain) {
        return CustomPermissionHandler.getInstance().getSecondaryOwnerSet(domain);
    }

    public static Map<CustomPermType, Set<PermUserDto>> getCustomUserPermMap(String objectName, String objectId,
                                                                             String permissionId,
                                                                             List<UserDto> userList) {
        return CustomPermissionHandler.getInstance().getCustomUserPermMap(objectName, objectId, permissionId, userList);
    }
}