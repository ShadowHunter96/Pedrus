package cz.bbn.cerberus.contracttype.ui.components.tab;

import com.vaadin.flow.component.checkbox.Checkbox;
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
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class ContractTypeDetailTabComponent extends VerticalLayout {

    private final AppBinderOperation<ContractTypeDto> binderOperation;
    private final boolean isDialog;

    public ContractTypeDetailTabComponent(AppBinderOperation<ContractTypeDto> binderOperation, boolean isDialog) {
        this.binderOperation = binderOperation;
        this.isDialog = isDialog;
        initComponent();
    }

    private void initComponent() {
        this.setSizeFull();

        if (isDialog) {
            setMargin(false);
            setPadding(false);
        }

        FormLayout formLayout = new FormLayout();
        TextField id = new TextField(Transl.get("Id"));
        id.setMaxLength(20);
        id.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        id.setReadOnly(binderOperation.getDto().getId() != null);
        binderOperation.getBinder().forField(id).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .withValidator(new KeyValidator())
                .withValidator(new MinMaxValidator(2, 20))
                .bind(ContractTypeDto::getId, ContractTypeDto::setId);
        formLayout.add(id);

        TextField name = new TextField(Transl.get("Name"));
        name.setMaxLength(100);
        name.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        binderOperation.getBinder().forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ContractTypeDto::getName, ContractTypeDto::setName);
        formLayout.add(name);

        TextArea description = new TextArea(Transl.get("Description"));
        description.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        binderOperation.getBinder().forField(description)
                .bind(ContractTypeDto::getDescription, ContractTypeDto::setDescription);
        description.setWidthFull();
        description.setHeight(CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());

        Checkbox allowed = new Checkbox(Transl.get("Allowed"));
        binderOperation.getBinder().forField(allowed).bind(ContractTypeDto::getAllowed, ContractTypeDto::setAllowed);

        Checkbox connectionRequired = new Checkbox(Transl.get("Connection required"));
        binderOperation.getBinder().forField(connectionRequired)
                .bind(ContractTypeDto::getConnectionRequired, ContractTypeDto::setConnectionRequired);

        Checkbox sales = new Checkbox(Transl.get("Sales"));
        binderOperation.getBinder().forField(sales).bind(ContractTypeDto::getSales, ContractTypeDto::setSales);

        Checkbox supplierCo = new Checkbox(Transl.get("Supplier contract"));
        binderOperation.getBinder().forField(supplierCo)
                .bind(ContractTypeDto::getSupplierCo, ContractTypeDto::setSupplierCo);

        Checkbox operational = new Checkbox(Transl.get("Operational"));
        binderOperation.getBinder().forField(operational)
                .bind(ContractTypeDto::getOperational, ContractTypeDto::setOperational);

        Checkbox employeeCo = new Checkbox(Transl.get("Employee contract"));
        binderOperation.getBinder().forField(employeeCo)
                .bind(ContractTypeDto::getEmployeeCo, ContractTypeDto::setEmployeeCo);

        add(formLayout, description, allowed, connectionRequired, sales, supplierCo, operational, employeeCo);

        if (binderOperation.getDto().getId() == null) {
            allowed.setValue(true);
        }

        if (!SecurityUtils.hasPermission(Permission.CONTRACT_TYPE_EDIT)) {
            id.setReadOnly(true);
            name.setReadOnly(true);
            description.setReadOnly(true);
            allowed.setReadOnly(true);
            connectionRequired.setReadOnly(true);
            sales.setReadOnly(true);
            supplierCo.setReadOnly(true);
            operational.setReadOnly(true);
            employeeCo.setReadOnly(true);
        }

        binderOperation.getBinder().setBean(binderOperation.getDto());

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);
    }
}
