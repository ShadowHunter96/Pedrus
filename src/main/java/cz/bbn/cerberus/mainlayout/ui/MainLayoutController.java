package cz.bbn.cerberus.mainlayout.ui;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.server.VaadinServletRequest;
import cz.bbn.cerberus.changelog.ChangelogService;
import cz.bbn.cerberus.changelog.ui.ChangelogDialog;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.enviromennt.AppProperty;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.role.RoleService;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.rolecustompermission.RoleCustomPermissionService;
import cz.bbn.cerberus.translation.enums.ApplicationTranslation;
import cz.bbn.cerberus.user.UserComponentOperation;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.UserValues;
import cz.bbn.cerberus.user.ui.UserInfoDialog;
import cz.bbn.cerberus.usermessage.UserMessageService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class MainLayoutController {

    private final ListService listService;
    private final CustomPermissionService customPermissionService;
    private final RoleCustomPermissionService roleCustomPermissionService;
    private final RoleService roleService;
    private final UserMessageService userMessageService;
    private final ChangelogService changelogService;
    private final AppEnv appEnv;
    private final UserComponentOperation userComponentOperation;
    private final UserService userService;
    private final UserValues userValues;
    private final Transl transl;

    public MainLayoutController(ListService listService, CustomPermissionService customPermissionService,
                                RoleCustomPermissionService roleCustomPermissionService, RoleService roleService, UserMessageService userMessageService,
                                ChangelogService changelogService, AppEnv appEnv,
                                UserComponentOperation userComponentOperation, UserService userService, UserValues userValues, Transl transl) {
        this.listService = listService;
        this.customPermissionService = customPermissionService;
        this.roleCustomPermissionService = roleCustomPermissionService;
        this.roleService = roleService;
        this.userMessageService = userMessageService;
        this.changelogService = changelogService;
        this.appEnv = appEnv;
        this.userComponentOperation = userComponentOperation;
        this.userService = userService;
        this.userValues = userValues;
        this.transl = transl;
    }

    public void reloadCash() {
        listService.reloadAll();
        customPermissionService.loadPermissionSetByCurrentUser();
        roleCustomPermissionService.loadAllPermissionSet();
        roleService.reloadRoleSet(SecurityUtils.getCurrentUser().getActiveRoleSet());
        transl.initTranslations();
        UI.getCurrent().getPage().reload();
    }

    public void logout() {
        if (SecurityUtils.userIsAzureInstance()) {
            UI.getCurrent().getPage().setLocation(appEnv.getStringProperty(AppProperty.AZURE_LOGOUT));
        }
        VaadinServletRequest.getCurrent().getHttpServletRequest().getSession().invalidate();
    }

    public void showUserInfoDialog(MainLayout mainLayout) {
        UserInfoDialog userInfoDialog = new UserInfoDialog(userMessageService, mainLayout, userComponentOperation, userValues);
        userInfoDialog.open();
    }

    public void showChangelogDialog() {
        ChangelogDialog changelogDialog = new ChangelogDialog(changelogService);
        changelogDialog.open();
    }

    public void changeLanguage(ApplicationTranslation language) {
        userService.setApplicationTranslation(language);
        UI.getCurrent().getPage().reload();
        log.info("language change to {}", UserService.getApplicationTranslation());
    }
}
