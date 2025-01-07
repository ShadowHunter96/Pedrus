package cz.bbn.cerberus.documenttype.ui.component.tab;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.checkbox.CheckboxGroup;
import com.vaadin.flow.component.checkbox.CheckboxGroupVariant;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.commons.validator.KeyValidator;
import cz.bbn.cerberus.commons.validator.MinMaxValidator;
import cz.bbn.cerberus.documenttype.dto.DocumentTypeDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;
import java.util.stream.Collectors;

public class DocumentTypeDetailTabComponent extends VerticalLayout {

    private final AppBinderOperation<DocumentTypeDto> appBinderOperation;
    private final boolean isDialog;
    private final List<EnumerationDto> allowedFormatsList;

    private final CheckboxGroup<EnumerationDto> allowedFormats;

    public DocumentTypeDetailTabComponent(AppBinderOperation<DocumentTypeDto> appBinderOperation,
                                          boolean isDialog, List<EnumerationDto> allowedFormatsList,
                                          CheckboxGroup<EnumerationDto> allowedFormats) {
        this.appBinderOperation = appBinderOperation;
        this.isDialog = isDialog;
        this.allowedFormatsList = allowedFormatsList;
        this.allowedFormats = allowedFormats;
        init();
    }

    private void init() {
        setSizeFull();

        FormLayout formLayout = new FormLayout();
        TextField id = new TextField(Transl.get("Id"));
        id.setMaxLength(20);
        id.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        id.setEnabled(appBinderOperation.getDto().getId() == null);
        appBinderOperation.getBinder().forField(id).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .withValidator(new KeyValidator())
                .withValidator(new MinMaxValidator(2, 20))
                .bind(DocumentTypeDto::getId, DocumentTypeDto::setId);
        formLayout.add(id);

        TextField name = new TextField(Transl.get("Name"));
        name.setMaxLength(100);
        name.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        appBinderOperation.getBinder().forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(DocumentTypeDto::getName, DocumentTypeDto::setName);
        formLayout.add(name);

        initAllowedFormats(appBinderOperation.getDto().getAllowedFormatsList());

        TextArea description = new TextArea(Transl.get("Description"));
        description.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        description.setWidthFull();
        description.setHeight(CssVariables.DEFAULT_FIELD_HEIGHT.getValue());
        appBinderOperation.getBinder().forField(description)
                .bind(DocumentTypeDto::getDescription, DocumentTypeDto::setDescription);

        Checkbox allowed = new Checkbox(Transl.get("Allowed"));
        appBinderOperation.getBinder().forField(allowed).bind(DocumentTypeDto::getAllowed, DocumentTypeDto::setAllowed);

        if (appBinderOperation.getDto().getId() == null) {
            allowed.setValue(true);
        }

        appBinderOperation.getBinder().setBean(appBinderOperation.getDto());

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);

        add(formLayout, allowedFormats, description, allowed);

        if (this.isDialog) {
            setMargin(false);
            setPadding(false);
        }

        if (!SecurityUtils.hasPermission(Permission.DOCUMENT_TYPE_EDIT)) {
            id.setReadOnly(true);
            name.setReadOnly(true);
            description.setReadOnly(true);
            allowed.setReadOnly(true);
            allowedFormats.setReadOnly(true);
        }
    }

    private void initAllowedFormats(List<Long> actualSelectedItemIdList) {
        allowedFormats.setItems(allowedFormatsList);
        allowedFormats.setItemLabelGenerator(EnumerationDto::getName);
        allowedFormats.setItemEnabledProvider(EnumerationDto::getAllowed);
        allowedFormats.addThemeVariants(CheckboxGroupVariant.LUMO_HELPER_ABOVE_FIELD);
        if (actualSelectedItemIdList != null) {
            allowedFormats.select(
                    allowedFormatsList
                            .stream()
                            .filter(enumerationDto -> actualSelectedItemIdList.contains(enumerationDto.getId()))
                            .collect(Collectors.toSet()));
        }
    }

}
