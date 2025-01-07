package cz.bbn.cerberus.area.ui.component.tab;

import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.value.ValueChangeMode;
import cz.bbn.cerberus.area.dto.AreaDto;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.commons.validator.KeyValidator;
import cz.bbn.cerberus.commons.validator.MinMaxValidator;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import org.vaadin.addons.badge.Badge;

public class AreaDetailTabComponent extends VerticalLayout {

    private final AppBinderOperation<AreaDto> appBinderOperation;
    private final boolean isDialog;
    private final HorizontalLayout viewLayout = new HorizontalLayout();

    public AreaDetailTabComponent(AppBinderOperation<AreaDto> appBinderOperation, boolean isDialog) {
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
                .bind(AreaDto::getId, AreaDto::setId);
        formLayout.add(id);

        if (appBinderOperation.getDto().getId() != null) {
            id.setReadOnly(true);
        }

        TextField name = new TextField(Transl.get("Name"));
        name.setValueChangeMode(ValueChangeMode.TIMEOUT);
        name.setValueChangeTimeout(1000);
        name.setMaxLength(100);
        name.addValueChangeListener(event ->
                VaadinComponents.generateBadgeViewLayout(viewLayout, event.getValue(),
                        appBinderOperation.getDto().getBadgeVariant(), appBinderOperation.getDto().getIcon()));

        appBinderOperation.getBinder().forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(AreaDto::getName, AreaDto::setName);
        formLayout.add(name);

        ComboBox<VaadinIcon> vaadinIcons = VaadinComponents.getVaadinIconComboBox();
        vaadinIcons.addValueChangeListener(event ->
                VaadinComponents.generateBadgeViewLayout(viewLayout, appBinderOperation.getDto().getName(),
                        appBinderOperation.getDto().getBadgeVariant(), event.getValue()));

        appBinderOperation.getBinder().forField(vaadinIcons).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(AreaDto::getIcon, AreaDto::setIcon);
        vaadinIcons.setMinWidth("15em");
        formLayout.add(vaadinIcons);

        ComboBox<Badge.BadgeVariant> badgeColorVariantEnumComboBox = VaadinComponents.getBadgeVariantComboBox();
        badgeColorVariantEnumComboBox.addValueChangeListener(event -> VaadinComponents.generateBadgeViewLayout(
                viewLayout,
                appBinderOperation.getDto().getName(), event.getValue(),
                appBinderOperation.getDto().getIcon()));

        appBinderOperation.getBinder().forField(badgeColorVariantEnumComboBox)
                .asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(AreaDto::getBadgeVariant, AreaDto::setBadgeVariant);
        formLayout.add(badgeColorVariantEnumComboBox);

        appBinderOperation.getBinder().setBean(appBinderOperation.getDto());

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);

        add(formLayout);
        add(viewLayout);

        TextArea description = new TextArea(Transl.get("Description"));
        description.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        appBinderOperation.getBinder().forField(description).bind(AreaDto::getDescription, AreaDto::setDescription);
        description.setWidthFull();
        description.setHeight(CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());
        add(description);

        if (isDialog) {
            setMargin(false);
            setPadding(false);
        }

        if (!SecurityUtils.hasPermission(Permission.AREA_EDIT)) {
            id.setReadOnly(true);
            name.setReadOnly(true);
            vaadinIcons.setReadOnly(true);
            badgeColorVariantEnumComboBox.setReadOnly(true);
        }

        VaadinComponents.generateBadgeViewLayout(viewLayout, appBinderOperation.getDto().getName(),
                appBinderOperation.getDto().getBadgeVariant(), appBinderOperation.getDto().getIcon());
    }

}
