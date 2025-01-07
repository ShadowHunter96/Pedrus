package cz.bbn.cerberus.enumeration.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.enumeration.dto.EnumerationFilterDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationTypeDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class EnumerationFilterComponent extends FormLayout {

    private final Button search;
    private final EnumerationTypeDto enumerationTypeDto;
    private TextField name;

    private Checkbox showNotAllowed;
    private Checkbox showDeleted;

    public EnumerationFilterComponent(Button search, EnumerationTypeDto enumerationTypeDto) {
        this.search = search;
        this.enumerationTypeDto = enumerationTypeDto;
        initComponent();
    }

    private void initComponent() {
        name = new TextField(Transl.get("Name"));
        this.add(name);

        showNotAllowed = new Checkbox(Transl.get("Show not allowed"));
        if (SecurityUtils.hasPermission(Permission.valueOfOrNotExists(
                enumerationTypeDto.getPermissionKey().concat("_SHOW_NOT_ALLOWED")))) {
            this.add(showNotAllowed);
        }
        showDeleted = new Checkbox(Transl.get("Show deleted"));
        if (SecurityUtils.hasPermission(Permission.valueOfOrNotExists(
                enumerationTypeDto.getPermissionKey().concat("_SHOW_DELETED")))) {
            this.add(showDeleted);
        }

        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    public EnumerationFilterDto getEnumerationFilterDto() {
        EnumerationFilterDto enumerationFilterDto = new EnumerationFilterDto();
        enumerationFilterDto.setName(name.getValue());
        enumerationFilterDto.setShowNotAllowed(showNotAllowed.getValue());
        enumerationFilterDto.setShowDeleted(showDeleted.getValue());
        return enumerationFilterDto;
    }
}
