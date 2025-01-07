package cz.bbn.cerberus.user;

import com.vaadin.flow.component.UI;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.role.RoleService;
import cz.bbn.cerberus.translation.Transl;
import org.springframework.stereotype.Component;

import java.util.Set;

@Component
public class UserComponentOperation {

    private final RoleService roleService;
    private final UserService userService;
    private final AppEnv appEnv;

    public UserComponentOperation(RoleService roleService, UserService userService, AppEnv appEnv) {
        this.roleService = roleService;
        this.userService = userService;
        this.appEnv = appEnv;
    }

    public void saveUserActiveRoleLSet(Set<String> userActiveRoleSet) {
        if (userActiveRoleSet.isEmpty()) {
            ErrorNotification.show(Transl.get("At least one role must be selected"), appEnv);
        } else {
            userService.updateActiveUserRole(SecurityUtils.getCurrentUserId(), userActiveRoleSet);
            roleService.reloadRoleSet(userActiveRoleSet);
            UI.getCurrent().getPage().reload();
        }
    }
}
