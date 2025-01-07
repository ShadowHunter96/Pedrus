package cz.bbn.cerberus.user.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserFilterDto;

public class UserFilterDtoComponent extends FormLayout {

    private TextField name;
    private Checkbox showDeleted;

    private final Button search;

    public UserFilterDtoComponent(Button search) {
        this.search = search;
        initComponents();
    }

    private void initComponents() {
        name = new TextField(Transl.get("Login name"));
        this.add(name);

        showDeleted = new Checkbox(Transl.get("Show deleted"));
        if (SecurityUtils.hasPermission(Permission.USER_SHOW_DELETED)) {
            this.add(showDeleted);
        }

        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    public UserFilterDto getUserFilterDto() {
        UserFilterDto userFilterDto = new UserFilterDto();
        userFilterDto.setName(name.getValue());
        userFilterDto.setShowDeleted(showDeleted.getValue());
        return userFilterDto;
    }
}
