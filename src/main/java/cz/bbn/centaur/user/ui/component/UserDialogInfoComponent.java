package cz.bbn.cerberus.user.ui.component;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.AppUser;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserValues;
import org.apache.commons.lang3.StringUtils;

public class UserDialogInfoComponent extends VerticalLayout {

    private MultiSelectComboBox<String> activeRole;
    private Checkbox turnOffTranslations;
    private final UserValues userValues;

    public UserDialogInfoComponent(UserValues userValues) {
        this.userValues = userValues;
    }

    public void initComponent() {
        AppUser appUser = SecurityUtils.getCurrentUser();
        this.setWidthFull();

        FormLayout formLayout = new FormLayout();
        formLayout.setSizeFull();
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);

        TextField login = new TextField(Transl.get("User login"));
        login.setReadOnly(true);
        formLayout.add(login);

        TextField userName = new TextField(Transl.get("User name"));
        userName.setReadOnly(true);
        formLayout.add(userName);

        TextField mail = new TextField(Transl.get("Mail"));
        mail.setReadOnly(true);
        formLayout.add(mail);

        FormLayout horizontalLayout = new FormLayout();
        horizontalLayout.add(formLayout);

        TextField role = new TextField(Transl.get("Roles"));
        role.setWidth("35em");
        role.setReadOnly(true);
        horizontalLayout.add(role);

        activeRole = new MultiSelectComboBox<>(Transl.get("Active roles"));
        horizontalLayout.add(activeRole);

        turnOffTranslations = new Checkbox(Transl.get("Turn off translations"));
        turnOffTranslations.setValue(userValues.isTurnOffTranslations());

        if(SecurityUtils.hasPermission(Permission.TURN_OFF_TRANSLATIONS)) {
            horizontalLayout.add(turnOffTranslations);
        }

        TextArea permissions = new TextArea(Transl.get("Rights"));
        permissions.setWidthFull();
        permissions.setReadOnly(true);
        this.add(horizontalLayout, permissions);

        if (appUser != null) {
            login.setValue(appUser.getUsername());
            userName.setValue(appUser.getName());
            mail.setValue(appUser.getMail());
            activeRole.setItems(appUser.getRoleSet());
            activeRole.setValue(appUser.getActiveRoleSet());
            role.setValue(StringUtils.join(appUser.getRoleSet(), ", "));
            permissions.setValue(StringUtils.join(appUser.getAuthorities(), ", "));
        }
    }

    public MultiSelectComboBox<String> getRole() {
        return activeRole;
    }

    public Checkbox getTurnOffTranslations() {
        return turnOffTranslations;
    }
}
