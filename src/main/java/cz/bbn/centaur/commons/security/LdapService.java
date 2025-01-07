package cz.bbn.cerberus.commons.security;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.enviromennt.AppProperty;
import cz.bbn.cerberus.user.UserService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ldap.core.AttributesMapper;
import org.springframework.ldap.core.LdapTemplate;
import org.springframework.ldap.core.support.LdapContextSource;
import org.springframework.ldap.filter.EqualsFilter;
import org.springframework.ldap.filter.Filter;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service
public class LdapService {

    private static final String MAIL = "mail";
    private static final String MEMBER_OF = "memberOf";
    private static final String START = "CN=";
    private static final String END = ",OU";

    private final AppEnv appEnv;
    private final AppLogService appLogService;
    private final UserService userService;

    private LdapTemplate ldapTemplate;

    public LdapService(AppEnv appEnv, AppLogService appLogService, UserService userService) {
        this.appEnv = appEnv;

        this.appLogService = appLogService;
        this.userService = userService;
        initContext();
    }

    public boolean authenticate(Authentication auth) {
        String name = auth.getName();
        String password = auth.getCredentials().toString();
        Filter filter = new EqualsFilter(MAIL, name);
        String base = appEnv.getStringProperty(AppProperty.LDAP_BASE);
        return ldapTemplate.authenticate(base, filter.encode(), password);
    }

    public AppUser getAppUserFromLdap(String mail) {
        Filter filter = new EqualsFilter(MAIL, mail);

        List<AppUser> users = ldapTemplate.search(appEnv.getStringProperty(AppProperty.LDAP_BASE), filter.encode(),
                (AttributesMapper<AppUser>) attributes -> {
                    appLogService.log("login", "ldap info: ".concat(attributes.toString()), "", 0L);
                    AppUser appUser = new AppUser(mail, attributes.get("name").get().toString(), mail);
                    appUser.setLdapGroup(attributes.get(MEMBER_OF).toString());
                    return appUser;
                });

        if (users.isEmpty() || StringUtils.isEmpty(users.get(0).getLdapGroup())
                || !users.get(0).getLdapGroup().contains(appEnv.getLdapPrefix())) {
            return null;
        }

        AppUser appUser = users.get(0);
        String group = appUser.getLdapGroup();
        Set<String> roleSet = new HashSet<>();

        Pattern pattern = Pattern.compile(START.concat("(.*?)").concat(END));
        Matcher matcher = pattern.matcher(group);
        while (matcher.find()) {
            String role = matcher.group(1);
            if (role.contains(appEnv.getLdapPrefix())) {
                roleSet.add(role.toUpperCase());
            }
        }
        if (roleSet.isEmpty()) {
            roleSet.add("");
        }
        appUser.setRoleSet(roleSet);
        return appUser;
    }

    private void initContext() {
        LdapContextSource contextSource = new LdapContextSource();
        contextSource.setUrl(appEnv.getStringProperty(AppProperty.LDAP_URL));
        contextSource.setUserDn(appEnv.getStringProperty(AppProperty.LDAP_USERDN));
        contextSource.setPassword(appEnv.getStringProperty(AppProperty.LDAP_PASSWORD));
        contextSource.afterPropertiesSet();

        ldapTemplate = new LdapTemplate(contextSource);
    }
}
