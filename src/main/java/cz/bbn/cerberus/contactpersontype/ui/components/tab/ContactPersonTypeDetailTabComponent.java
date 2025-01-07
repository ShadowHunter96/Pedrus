package cz.bbn.cerberus.contactpersontype.ui.components.tab;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.commons.validator.KeyValidator;
import cz.bbn.cerberus.commons.validator.MinMaxValidator;
import cz.bbn.cerberus.contactpersontype.dto.ContactPersonTypeDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class ContactPersonTypeDetailTabComponent extends VerticalLayout {

    private final AppBinderOperation<ContactPersonTypeDto> binderOperation;
    private final boolean isDialog;

    public ContactPersonTypeDetailTabComponent(AppBinderOperation<ContactPersonTypeDto> binderOperation,
                                               boolean isDialog) {
        this.binderOperation = binderOperation;
        this.isDialog = isDialog;
        init();
    }

    private void init() {
        this.setSizeFull();

        if (isDialog) {
            setMargin(false);
            setPadding(false);
        }

        FormLayout formLayout = new FormLayout();
        TextField id = new TextField(Transl.get("Id"));
        id.setMaxLength(20);
        id.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        id.setEnabled(binderOperation.getDto().getId() == null);
        binderOperation.getBinder().forField(id).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .withValidator(new KeyValidator())
                .withValidator(new MinMaxValidator(2, 20))
                .bind(ContactPersonTypeDto::getId, ContactPersonTypeDto::setId);
        formLayout.add(id);

        TextField name = new TextField(Transl.get("Name"));
        name.setMaxLength(100);
        name.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        binderOperation.getBinder().forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ContactPersonTypeDto::getName, ContactPersonTypeDto::setName);
        formLayout.add(name);

        Checkbox allowed = new Checkbox(Transl.get("Allowed"));
        binderOperation.getBinder().forField(allowed)
                .bind(ContactPersonTypeDto::getAllowed, ContactPersonTypeDto::setAllowed);

        add(formLayout, allowed);

        if (binderOperation.getDto().getId() == null) {
            allowed.setValue(true);
        }

        if (!SecurityUtils.hasPermission(Permission.CONTACT_PERSON_TYPE_EDIT)) {
            id.setReadOnly(true);
            name.setReadOnly(true);
            allowed.setReadOnly(true);
        }

        binderOperation.getBinder().setBean(binderOperation.getDto());

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);
    }
}
