package cz.bbn.cerberus.commons.security;

import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.role.dto.RoleHasPermissionDto;
import cz.bbn.cerberus.user.dto.UserDto;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.oauth2.core.oidc.OidcIdToken;
import org.springframework.security.oauth2.core.oidc.OidcUserInfo;
import org.springframework.security.oauth2.core.oidc.user.DefaultOidcUser;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;


public class AppUser extends DefaultOidcUser implements UserDetails {

    private String bid;

    private String name;

    private String mail;

    private Set<Permission> permissionSet = new HashSet<>();

    private Set<String> roleSet;

    private Set<String> activeRoleSet;

    private String ldapGroup;

    private Long id;

    private boolean azureLogin;

    private String azureBearerToken;

    private String azureId;

    public AppUser(String bid, String name, String email) {
        super(null, createDefaultToken());
        this.bid = bid;
        this.name = name;
        this.mail = email;
    }

    public AppUser(Collection<? extends GrantedAuthority> authorities, OidcIdToken idToken, OidcUserInfo userInfo) {
        super(authorities, idToken, userInfo);
        String username = idToken.getClaim("preferred_username");
        this.bid = username;
        this.name = idToken.getClaim("name");
        this.mail = username;
        this.azureLogin = true;
    }

    public AppUser(UserDto userDto) {
        super(null, createDefaultToken());
        this.bid = userDto.getLogin();
        this.name = userDto.getName();
        this.mail = userDto.getMail();
        this.setActiveRoleSet(Arrays.stream(userDto.getUserRoles()
                .split(";")).collect(Collectors.toSet()));
        this.id = userDto.getId();
    }

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
        return permissionSet;
    }

    public void setPermissionSet(Set<Permission> permissionSet) {
        this.permissionSet = permissionSet;
    }

    @Override
    public String getPassword() {
        return null;
    }

    @Override
    public String getUsername() {
        return getBid();
    }

    @Override
    public boolean isAccountNonExpired() {
        return true;
    }

    @Override
    public boolean isAccountNonLocked() {
        return true;
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return true;
    }

    @Override
    public boolean isEnabled() {
        return true;
    }

    public String getBid() {
        return bid;
    }

    public void setBid(String principal) {
        this.bid = principal;
    }

    public void setByUserDto(UserDto userDto, Set<RoleDto> roleDtoSet, Set<RoleDto> visibleRoleDtoSet) {
        boolean deleted = Boolean.TRUE.equals(userDto.getDeleted());
        this.id = deleted ? 0 : userDto.getId();
        roleSet = new HashSet<>();
        activeRoleSet = new HashSet<>();
        permissionSet = new HashSet<>();
        if (!deleted) {
            roleDtoSet.forEach(roleDto -> roleSet.add(roleDto.getId()));
            visibleRoleDtoSet.forEach(roleDto -> {
                activeRoleSet.add(roleDto.getId());
                permissionSet.addAll(convertToPermissions(roleDto.getRoleHasPermissionSet()));
            });
        } else {
            roleSet.add("");
            activeRoleSet.add("");
        }
        this.name = userDto.getName();
        this.mail = userDto.getMail();
        this.azureId = userDto.getAzureId();
    }

    public void setActiveRoleSet(Set<String> activeRoleSet) {
        this.activeRoleSet = activeRoleSet;
    }

    public Set<String> getActiveRoleSet() {
        return activeRoleSet;
    }

    public Long getId() {
        return id;
    }

    @Override
    public String getName() {
        return name;
    }

    public boolean isAzureLogin() {
        return azureLogin;
    }

    public String getMail() {
        return mail;
    }

    public void setMail(String mail) {
        this.mail = mail;
    }

    public void setLdapGroup(String ldapGroup) {
        this.ldapGroup = ldapGroup;
    }

    public String getLdapGroup() {
        return ldapGroup;
    }

    public void setRoleSet(Set<String> roleSet) {
        this.roleSet = roleSet;
    }

    private Set<Permission> convertToPermissions(Set<RoleHasPermissionDto> roleHasPermissionDtoSet) {
        Set<Permission> actualPermissionSet = new HashSet<>();
        roleHasPermissionDtoSet.forEach(roleHasPermissionDto -> {
            Permission permission = Permission.valueOfOrNotExists(roleHasPermissionDto.getPermissionId());
            if (permission != Permission.NON_EXISTENT_PERMISSION) {
                actualPermissionSet.add(permission);
            }
        });
        return actualPermissionSet;
    }

    public Set<String> getRoleSet() {
        return roleSet;
    }

    private static OidcIdToken createDefaultToken() {
        // pouzivate se pro vytvoreni usera bez azure prihlaseni. Jinak by vytvareni tridy padalo do vyjimky
        return OidcIdToken.withTokenValue("Token").claim("ia", "").claim("sub", "").build();
    }

    public String getAzureBearerToken() {
        return azureBearerToken;
    }

    public void setAzureBearerToken(String azureBearerToken) {
        this.azureBearerToken = azureBearerToken;
    }

    public void setAzureId(String azureId) {
        this.azureId = azureId;
    }

    public String getAzureId() {
        return azureId;
    }

    public void setId(Long id) {
        this.id = id;
    }
}
