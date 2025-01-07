package cz.bbn.cerberus.dph.ui.component.tab;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.NumberField;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.commons.validator.KeyValidator;
import cz.bbn.cerberus.commons.validator.MinMaxValidator;
import cz.bbn.cerberus.dph.dto.DphDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class DphDetailTabComponent extends VerticalLayout {

    private final AppBinderOperation<DphDto> appBinderOperation;
    private final boolean isDialog;

    public DphDetailTabComponent(AppBinderOperation<DphDto> appBinderOperation, boolean isDialog) {
        this.appBinderOperation = appBinderOperation;
        this.isDialog = isDialog;
        initComponent();
    }

    private void initComponent() {
        setSizeFull();

        FormLayout formLayout = new FormLayout();
        TextField id = new TextField(Transl.get("Id"));
        id.setMaxLength(20);
        appBinderOperation.getBinder().forField(id).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .withValidator(new KeyValidator())
                .withValidator(new MinMaxValidator(2, 20))
                .bind(DphDto::getId, DphDto::setId);
        formLayout.add(id);

        TextField name = new TextField(Transl.get("Name"));
        name.setMaxLength(100);
        appBinderOperation.getBinder().forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(DphDto::getName, DphDto::setName);
        formLayout.add(name);

        NumberField value = new NumberField(Transl.get("Value"));
        appBinderOperation.getBinder().forField(value).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(DphDto::getValue, DphDto::setValue);
        value.setPlaceholder(Transl.get("In 0.xx format"));
        formLayout.add(value);

        Checkbox defaultValue = new Checkbox(Transl.get("Default value"));
        appBinderOperation.getBinder().forField(defaultValue).bind(DphDto::getDefaultValue, DphDto::setDefaultValue);
        formLayout.add(defaultValue);

        Checkbox deleted = new Checkbox(Transl.get("Allowed"));
        if (appBinderOperation.getDto().getId() != null) {
            appBinderOperation.getBinder().forField(deleted).bind(DphDto::getAllowed, DphDto::setAllowed);
            formLayout.add(deleted);
        }

        appBinderOperation.getBinder().setBean(appBinderOperation.getDto());

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);

        add(formLayout);

        if (appBinderOperation.getDto().getId() != null) {
            DphDto dphDto = appBinderOperation.getDto();
            id.setReadOnly(true);
            name.setReadOnly(true);
            value.setReadOnly(true);
            if (Boolean.TRUE.equals(dphDto.getAllowed())) {
                deleted.setReadOnly(true);
            }
        }

        if (isDialog) {
            setMargin(false);
            setPadding(false);
        }

        if (!SecurityUtils.hasPermission(Permission.DPH_EDIT)) {
            id.setReadOnly(true);
            name.setReadOnly(true);
            value.setReadOnly(true);
            defaultValue.setReadOnly(true);
            deleted.setReadOnly(true);
        }
    }


}
