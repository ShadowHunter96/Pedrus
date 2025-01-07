package cz.bbn.cerberus.assetposition.ui.component.tab;

import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.assetposition.dto.AssetPositionDto;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.component.ui.map.AppMap;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.validator.KeyValidator;
import cz.bbn.cerberus.commons.validator.MinMaxValidator;
import cz.bbn.cerberus.translation.Transl;

public class AssetPositionDetailTabComponent extends VerticalLayout {

    private final AppBinderOperation<AssetPositionDto> appBinderOperation;
    private final boolean isDialog;
    private final AppEnv appEnv;

    public AssetPositionDetailTabComponent(AppBinderOperation<AssetPositionDto> appBinderOperation, boolean isDialog,
                                           AppEnv appEnv) {
        this.appBinderOperation = appBinderOperation;
        this.isDialog = isDialog;
        this.appEnv = appEnv;
        init();
    }

    private void init() {
        setSizeFull();

        FormLayout formLayout = new FormLayout();
        TextField id = new TextField(Transl.get("Id"));
        id.setMaxLength(20);
        appBinderOperation.getBinder().forField(id).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .withValidator(new KeyValidator())
                .withValidator(new MinMaxValidator(2, 20))
                .bind(AssetPositionDto::getId, AssetPositionDto::setId);
        formLayout.add(id);

        TextField name = new TextField(Transl.get("Name"));
        name.setMaxLength(100);
        appBinderOperation.getBinder().forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(AssetPositionDto::getName, AssetPositionDto::setName);
        formLayout.add(name);

        TextField longitude = new TextField(Transl.get("Longitude"));
        longitude.setMaxLength(30);
        appBinderOperation.getBinder().forField(longitude)
                .bind(AssetPositionDto::getLongitude, AssetPositionDto::setLongitude);
        formLayout.add(longitude);

        TextField latitude = new TextField(Transl.get("Latitude"));
        latitude.setMaxLength(30);
        appBinderOperation.getBinder().forField(latitude)
                .bind(AssetPositionDto::getLatitude, AssetPositionDto::setLatitude);
        formLayout.add(latitude);

        appBinderOperation.getBinder().setBean(appBinderOperation.getDto());

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);

        add(formLayout);

        AppMap appMap = new AppMap(longitude, latitude, appEnv);
        appMap.initMap();
        add(appMap);

        TextArea description = new TextArea(Transl.get("Description"));
        description.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        appBinderOperation.getBinder().forField(description)
                .bind(AssetPositionDto::getDescription, AssetPositionDto::setDescription);
        description.setWidthFull();
        description.setHeight(CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());
        add(description);

        if (isDialog) {
            setMargin(false);
            setPadding(false);
        }
    }
}
