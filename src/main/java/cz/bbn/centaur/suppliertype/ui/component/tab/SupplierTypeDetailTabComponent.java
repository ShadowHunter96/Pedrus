package cz.bbn.cerberus.suppliertype.ui.component.tab;

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
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.suppliertype.dto.SupplierTypeDto;
import cz.bbn.cerberus.translation.Transl;

public class SupplierTypeDetailTabComponent extends VerticalLayout {

    private final AppBinderOperation<SupplierTypeDto> binderOperation;
    private final boolean isDialog;

    public SupplierTypeDetailTabComponent(AppBinderOperation<SupplierTypeDto> binderOperation, boolean isDialog) {
        this.binderOperation = binderOperation;
        this.isDialog = isDialog;
        init();
    }

    private void init() {
        this.setSizeFull();

        FormLayout formLayout = new FormLayout();
        TextField id = new TextField(Transl.get("Id"));
        id.setMaxLength(20);
        id.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        id.setEnabled(binderOperation.getDto().getId() == null);
        binderOperation.getBinder().forField(id).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .withValidator(new KeyValidator())
                .withValidator(new MinMaxValidator(2, 20))
                .bind(SupplierTypeDto::getId, SupplierTypeDto::setId);
        formLayout.add(id);

        TextField name = new TextField(Transl.get("Name"));
        name.setMaxLength(100);
        name.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        binderOperation.getBinder().forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(SupplierTypeDto::getName, SupplierTypeDto::setName);
        formLayout.add(name);

        Checkbox allowed = new Checkbox(Transl.get("Allowed"));
        binderOperation.getBinder().forField(allowed).bind(SupplierTypeDto::getAllowed, SupplierTypeDto::setAllowed);

        add(formLayout, allowed);

        if (binderOperation.getDto().getId() == null) {
            allowed.setValue(true);
        }

        binderOperation.getBinder().setBean(binderOperation.getDto());

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);

        if (isDialog) {
            setMargin(false);
            setPadding(false);
        }

        if (!SecurityUtils.hasPermission(Permission.SUPPLIER_TYPE_EDIT)) {
            id.setReadOnly(true);
            name.setReadOnly(true);
            allowed.setReadOnly(true);
        }
    }
}
