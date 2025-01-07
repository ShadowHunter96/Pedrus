package cz.bbn.cerberus.technology.ui.component.tab;

import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.value.ValueChangeMode;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.commons.validator.KeyValidator;
import cz.bbn.cerberus.commons.validator.MinMaxValidator;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.technology.dto.TechnologyDto;
import cz.bbn.cerberus.translation.Transl;
import org.vaadin.addons.badge.Badge;

public class TechnologyDetailTabComponent extends VerticalLayout {

    private final AppBinderOperation<TechnologyDto> appBinderOperation;
    private final boolean isDialog;

    private final HorizontalLayout viewLayout = new HorizontalLayout();

    public TechnologyDetailTabComponent(AppBinderOperation<TechnologyDto> appBinderOperation, boolean isDialog) {
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
                .bind(TechnologyDto::getId, TechnologyDto::setId);
        formLayout.add(id);

        if (appBinderOperation.getDto().getId() != null) {
            id.setReadOnly(true);
        }

        TextField name = new TextField(Transl.get("Name"));
        name.setValueChangeMode(ValueChangeMode.TIMEOUT);
        name.setMaxLength(100);
        name.setValueChangeTimeout(1000);
        name.addValueChangeListener(event ->
                VaadinComponents.generateBadgeViewLayout(viewLayout, event.getValue(),
                        appBinderOperation.getDto().getBadgeVariant(), appBinderOperation.getDto().getIcon()));

        appBinderOperation.getBinder().forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(TechnologyDto::getName, TechnologyDto::setName);
        formLayout.add(name);

        appBinderOperation.getBinder().setBean(appBinderOperation.getDto());

        ComboBox<VaadinIcon> vaadinIcons = VaadinComponents.getVaadinIconComboBox();
        vaadinIcons.addValueChangeListener(event ->
                VaadinComponents.generateBadgeViewLayout(viewLayout, appBinderOperation.getDto().getName(),
                        appBinderOperation.getDto().getBadgeVariant(), event.getValue()));

        appBinderOperation.getBinder().forField(vaadinIcons).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(TechnologyDto::getIcon, TechnologyDto::setIcon);
        vaadinIcons.setMinWidth("15em");
        formLayout.add(vaadinIcons);

        ComboBox<Badge.BadgeVariant> badgeColorVariantEnumComboBox = VaadinComponents.getBadgeVariantComboBox();
        badgeColorVariantEnumComboBox.addValueChangeListener(event -> VaadinComponents.generateBadgeViewLayout(
                viewLayout, appBinderOperation.getDto().getName(), event.getValue(),
                appBinderOperation.getDto().getIcon()));

        appBinderOperation.getBinder().forField(badgeColorVariantEnumComboBox)
                .asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(TechnologyDto::getBadgeVariant, TechnologyDto::setBadgeVariant);
        badgeColorVariantEnumComboBox.setWidthFull();
        formLayout.add(badgeColorVariantEnumComboBox);

        add(formLayout);
        add(viewLayout);

        TextArea description = new TextArea(Transl.get("Description"));
        description.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        appBinderOperation.getBinder().forField(description)
                .bind(TechnologyDto::getDescription, TechnologyDto::setDescription);
        description.setWidthFull();
        description.setHeight(CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());
        add(description);

        if (isDialog) {
            setMargin(false);
            setPadding(false);
        }

        if (!SecurityUtils.hasPermission(Permission.TECHNOLOGY_EDIT)) {
            id.setReadOnly(true);
            name.setReadOnly(true);
            vaadinIcons.setReadOnly(true);
            badgeColorVariantEnumComboBox.setReadOnly(true);
            description.setReadOnly(true);
        }

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);
        VaadinComponents.generateBadgeViewLayout(viewLayout, appBinderOperation.getDto().getName(),
                appBinderOperation.getDto().getBadgeVariant(),
                appBinderOperation.getDto().getIcon());
    }


}
