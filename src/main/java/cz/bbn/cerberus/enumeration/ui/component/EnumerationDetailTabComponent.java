package cz.bbn.cerberus.enumeration.ui.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.checkbox.CheckboxGroup;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.ValidationResult;
import com.vaadin.flow.data.binder.Validator;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.enumeration.EnumerationComponentOperation;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class EnumerationDetailTabComponent extends VerticalLayout {

    private final AppBinderOperation<EnumerationDto> appBinderOperation;
    private final boolean isDialog;
    private final EnumerationComponentOperation enumerationComponentOperation;

    public EnumerationDetailTabComponent(AppBinderOperation<EnumerationDto> appBinderOperation, boolean isDialog,
                                         EnumerationComponentOperation enumerationComponentOperation) {
        this.appBinderOperation = appBinderOperation;
        this.isDialog = isDialog;
        this.enumerationComponentOperation = enumerationComponentOperation;
        initComponent();
    }

    private void initComponent() {
        setSizeFull();

        FormLayout formLayout = new FormLayout();
        TextField id = new TextField(Transl.get("Id"));
        id.setReadOnly(true);
        id.setValue(String.valueOf(appBinderOperation.getDto().getId()));
        formLayout.add(id);

        TextField name = new TextField(Transl.get("Name"));
        name.setMaxLength(100);
        appBinderOperation.getBinder().forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(EnumerationDto::getName, EnumerationDto::setName);
        formLayout.add(name);

        Checkbox allowed = new Checkbox(Transl.get("Allowed"));
        Component value = generateValue(id, allowed);
        if (value != null) {
            formLayout.add(value);
        }

        if (appBinderOperation.getDto().getId() != null) {
            appBinderOperation.getBinder().forField(allowed)
                    .bind(EnumerationDto::getAllowed, EnumerationDto::setAllowed);
            formLayout.add(allowed);
        }

        appBinderOperation.getBinder().setBean(appBinderOperation.getDto());

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);

        add(formLayout);

        TextArea description = new TextArea(Transl.get("Description"));
        description.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        appBinderOperation.getBinder().forField(description)
                .bind(EnumerationDto::getDescription, EnumerationDto::setDescription);
        description.setWidthFull();
        description.setWidth("calc(60em - 10px)");
        description.setHeight("15em");
        description.setMaxWidth("calc(100% - 10px)");
        description.setMaxHeight("100%");
        this.add(description);

        if (appBinderOperation.getDto().getId() == null) {
            id.setVisible(false);
        }

        if (isDialog) {
            setMargin(false);
            setPadding(false);
        }
    }

    private Component generateValue(TextField id, Checkbox allowed) {
        String enumerationTypeId = appBinderOperation.getDto().getEnumerationTypeDto().getId();
        switch (enumerationTypeId) {
            case "DOCUMENT_FORMAT":
                TextField textFieldValue = new TextField(Transl.get("Value"));
                textFieldValue.setMaxLength(255);
                textFieldValue.setHelperText(Transl.get("Comma separated values, for example xlt,xls,xst"));
                appBinderOperation.getBinder().forField(textFieldValue)
                        .asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                        .bind(EnumerationDto::getValue, EnumerationDto::setValue);
                return textFieldValue;
            case "ACTIVITY":
                // tady pouzivam CheckBoxGroup, protoze na rozdil od checkboxu ukazuje chybove hlasky
                CheckboxGroup<String> checkboxDefault = new CheckboxGroup();
                checkboxDefault.setItems(Transl.get("Default value"));
                appBinderOperation.getBinder().forField(checkboxDefault)
                        .withValidator(getValidator(id, enumerationTypeId, allowed))
                        .bind(enumerationDto -> enumerationDto.getValue().equalsIgnoreCase("true") ?
                                        new HashSet<>(Collections.singleton(Transl.get("Default value"))) : new HashSet<>(),
                                (enumerationDto, aBoolean) -> enumerationDto.setValue(aBoolean.isEmpty() ? "false" : "true"));
                return checkboxDefault;
            case "SUBNET":
                if (appBinderOperation.getDto().getId() == null) {
                    appBinderOperation.getDto().setValue("");
                }
                TextField textField = new TextField(Transl.get("Value"));
                textField.setMaxLength(255);
                appBinderOperation.getBinder().forField(textField).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                        .bind(EnumerationDto::getValue, EnumerationDto::setValue);
                return textField;
            default:
                return null;
        }
    }

    private Validator<? super Set<String>> getValidator(TextField id, String enumerationTypeId, Checkbox allowed) {
        return (value, valueContext) -> {
            if (value.stream().findAny().isPresent()) {
                Long longId = id != null && !id.getValue().equalsIgnoreCase("null")
                        ? Long.parseLong(id.getValue()) : 0L;
                List<EnumerationDto> enumerationDtoList =
                        enumerationComponentOperation.getByDefaultValueTrueList(longId, enumerationTypeId);
                if (!enumerationDtoList.isEmpty() && allowed.getValue()) {
                    return ValidationResult.error(Transl.get(
                            ErrorCode.DEFAULT_VALUE_ASSIGNED.getError(), enumerationDtoList.get(0).getName()));
                } else {
                    return ValidationResult.ok();
                }
            } else {
                return ValidationResult.ok();
            }
        };
    }


}
