package cz.bbn.cerberus.role.ui.component;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppDetailCardComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.validator.MinMaxValidator;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.translation.Transl;

public class RoleDetailComponent extends AppDetailCardComponent<RoleDto> {

    private static final String ROLE_CODE = "Role code";

    public RoleDetailComponent(
            RoleDto dto,
            SaveAction<RoleDto> saveAction,
            boolean showSaveButton,
            String backRoute,
            AppEnv appEnv,
            EntityNewComponentOperation entityNewComponentOperation
    ) {
        super(dto, saveAction, showSaveButton, appEnv, entityNewComponentOperation);

        this.addBackButton(backRoute);
        if (showSaveButton) {
            this.addSaveButton();
        }
        String headingText = dto.getId() == null ?
                Transl.get("Add role") :
                Transl.get("Role detail").concat(" - ").concat(dto.getId());
        this.setHeading(headingText);
        this.setId(RobotFrameworkVariables.ROLE_DETAIL_CARD_ID.getValue());
        this.setSizeFull();
    }

    @Override
    public void initComponent() {
        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.setSizeFull();
        FormLayout formLayout = new FormLayout();
        formLayout.setSizeFull();

        TextField code = new TextField(Transl.get(ROLE_CODE));
        code.setEnabled(getDto().getId() == null);
        code.setMaxLength(20);
        getBinder()
                .forField(code)
                .withValidator(new MinMaxValidator(2, 20))
                .asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY)).bind(RoleDto::getId, RoleDto::setId);

        TextField description = new TextField(Transl.get("Role name"));
        description.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        getBinder().forField(description)
                .asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(RoleDto::getDescription, RoleDto::setDescription);

        Checkbox backOffice = new Checkbox(Transl.get("Back office"));
        getBinder().forField(backOffice)
                .bind(RoleDto::getBackOffice, RoleDto::setBackOffice);

        Checkbox infrastructure = new Checkbox(Transl.get("Infrastructure"));
        getBinder().forField(infrastructure).bind(RoleDto::getInfrastructure, RoleDto::setInfrastructure);

        getBinder().setBean(getDto());

        formLayout.add(code, description, backOffice, infrastructure);

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);

        verticalLayout.add(formLayout);

        this.add(verticalLayout);
    }

}
