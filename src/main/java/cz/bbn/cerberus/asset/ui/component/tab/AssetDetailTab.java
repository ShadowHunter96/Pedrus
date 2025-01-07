package cz.bbn.cerberus.asset.ui.component.tab;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.asset.AssetComponentOperation;
import cz.bbn.cerberus.asset.dto.AssetDto;
import cz.bbn.cerberus.asset.dto.AssetStateEnum;
import cz.bbn.cerberus.asset.dto.PriceCategoryEnum;
import cz.bbn.cerberus.asset.dto.ProcurementEnum;
import cz.bbn.cerberus.assetposition.dto.AssetPositionDto;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.field.AppMoneyField;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.component.ui.map.AppMap;
import cz.bbn.cerberus.commons.component.ui.tab.TabDtoComponent;
import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

public class AssetDetailTab extends TabDtoComponent<AssetDto> {

    private final boolean isDialog;

    private final List<AssetPositionDto> assetPositionDtoList;
    private final List<SubjectDto> subjectList;
    private final List<EmployeeDto> employeeList;
    private final List<EnumerationDto> assetTypeList;
    private final AssetComponentOperation assetComponentOperation;
    private final boolean readOnly;

    public AssetDetailTab(AssetDto dto, SaveAction<AssetDto> saveAction,
                          AppEnv appEnv, List<AssetPositionDto> assetPositionDtoList, boolean isDialog,
                          boolean readOnly, List<SubjectDto> subjectList, List<EmployeeDto> employeeList,
                          List<EnumerationDto> assetTypeList, AssetComponentOperation assetComponentOperation) {
        super(dto, saveAction, appEnv);
        this.assetPositionDtoList = assetPositionDtoList;
        AssetPositionDto assetPositionDto = new AssetPositionDto();
        assetPositionDto.setId("own");
        assetPositionDto.setName(Transl.get("Own GPS coordinates"));
        this.assetPositionDtoList.add(0, assetPositionDto);
        this.subjectList = subjectList;
        this.employeeList = employeeList;
        this.assetTypeList = assetTypeList;
        this.isDialog = isDialog;
        this.readOnly = readOnly;
        this.assetComponentOperation = assetComponentOperation;
        initTab();
    }

    @Override
    protected void initTab() {
        removeAll();

        setMargin(false);
        setPadding(false);

        this.setId(RobotFrameworkVariables.ASSET_DETAIL_CARD_ID.getValue());
        this.setSizeFull();

        FormLayout formLayout = new FormLayout();

        TextField pohodaId = new TextField(Transl.get("Pohoda Id"));
        pohodaId.setMaxLength(40);
        pohodaId.setReadOnly(getDto().getId() == null);
        getBinder().forField(pohodaId).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(AssetDto::getPohodaId, AssetDto::setPohodaId);
        formLayout.add(pohodaId);

        TextField id = new TextField(Transl.get("Id"));
        id.setVisible(getDto().getId() == null);
        id.setReadOnly(true);
        id.setMaxLength(20);
        getBinder().forField(id).bind(AssetDto::getId, AssetDto::setId);
        formLayout.add(id);

        ComboBox<SubjectDto> subjectComboBox = new ComboBox<>(Transl.get("Our company"));
        subjectComboBox.setItems(subjectList);
        subjectComboBox.setItemLabelGenerator(SubjectDto::getName);
        subjectComboBox.setReadOnly(getDto().getId() == null);
        getBinder().forField(subjectComboBox).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(AssetDto::getOurCompany, AssetDto::setOurCompany);
        formLayout.add(subjectComboBox);

        TextField type = new TextField(Transl.get("Type"));
        type.setMaxLength(100);
        getBinder().forField(type).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY)).bind(AssetDto::getType, AssetDto::setType);
        type.setReadOnly(getDto().getId() == null);
        formLayout.add(type);

        ComboBox<ProcurementEnum> procurement = new ComboBox<>(Transl.get("Procurement"));
        procurement.setItems(ProcurementEnum.values());
        procurement.setItemLabelGenerator(ProcurementEnum::translate);
        getBinder().forField(procurement).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(AssetDto::getProcurement, AssetDto::setProcurement);
        procurement.setReadOnly(getDto().getId() == null);
        formLayout.add(procurement);

        DatePicker buyDate = VaadinComponents.getDatePicker(getDto().getBuyDate());
        buyDate.setLabel(Transl.get("Procurement date"));
        getBinder().forField(buyDate).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(AssetDto::getBuyDate, AssetDto::setBuyDate);
        buyDate.setReadOnly(getDto().getId() == null);
        formLayout.add(buyDate);

        TextField name = new TextField(Transl.get("Name"));
        name.setMaxLength(100);
        getBinder().forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(AssetDto::getName, AssetDto::setName);
        formLayout.add(name);

        TextField serialNumber = new TextField(Transl.get("Serial number"));
        serialNumber.setMaxLength(100);
        getBinder().forField(serialNumber).bind(AssetDto::getSerialNumber, AssetDto::setSerialNumber);
        formLayout.add(serialNumber);

        ComboBox<EmployeeDto> employee = new ComboBox<>(Transl.get("Responsible person"));
        employee.setItems(employeeList);
        employee.setItemLabelGenerator(this::getEmployeeName);
        getBinder().forField(employee).bind(AssetDto::getResponsiblePerson, AssetDto::setResponsiblePerson);
        formLayout.add(employee);

        TextField location = new TextField(Transl.get("Location"));
        location.setMaxLength(100);
        getBinder().forField(location).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(AssetDto::getLocation, AssetDto::setLocation);
        location.setReadOnly(getDto().getId() == null);
        formLayout.add(location);

        TextField longitude = new TextField(Transl.get("Longitude"));
        longitude.setMaxLength(60);
        longitude.setReadOnly(!SecurityUtils.hasPermission(Permission.CAN_CHANGE_ASSET_LOCATION));
        getBinder().forField(longitude)
                .bind(AssetDto::getLongitude, AssetDto::setLongitude);

        TextField latitude = new TextField(Transl.get("Latitude"));
        latitude.setMaxLength(60);
        latitude.setReadOnly(!SecurityUtils.hasPermission(Permission.CAN_CHANGE_ASSET_LOCATION));
        getBinder().forField(latitude).bind(AssetDto::getLatitude, AssetDto::setLatitude);

        AppMap appMap = new AppMap(longitude, latitude, getApptEnv());

        ComboBox<AssetPositionDto> assetPosition = new ComboBox<>(Transl.get("Choose destination"));
        assetPosition.setReadOnly(!SecurityUtils.hasPermission(Permission.CAN_CHANGE_ASSET_LOCATION));
        assetPosition.setItemLabelGenerator(AssetPositionDto::getName);
        assetPosition.setItems(assetPositionDtoList);

        if (getDto().getAssetPositionDto() == null && StringUtils.isNoneEmpty(getDto().getLatitude())) {
            assetPosition.setValue(assetPositionDtoList.get(0));
            getDto().setAssetPositionDto(assetPositionDtoList.get(0));
        } else if (getDto().getAssetPositionDto() != null) {
            assetPosition.setValue(getDto().getAssetPositionDto());
        }

        assetPosition.addValueChangeListener(valueChange -> {
            if ("own".equals(valueChange.getValue().getId())) {
                longitude.setReadOnly(false);
                latitude.setReadOnly(false);
                longitude.setValue(getDto().getLongitude());
                latitude.setValue(getDto().getLatitude());
                appMap.setLocationEditable(true);
            } else {
                longitude.setValue(valueChange.getValue().getLongitude());
                latitude.setValue(valueChange.getValue().getLatitude());
                longitude.setReadOnly(true);
                latitude.setReadOnly(true);
                appMap.initMap();
                appMap.setLocationEditable(false);
            }
        });

        if (getDto().getAssetPositionDto() != null && StringUtils.isNoneEmpty(getDto().getAssetPositionDto().getId())) {
            if ("own".equals(getDto().getAssetPositionDto().getId())) {
                longitude.setReadOnly(false);
                latitude.setReadOnly(false);
                appMap.setLocationEditable(true);
            } else {
                longitude.setReadOnly(true);
                latitude.setReadOnly(true);
                appMap.setLocationEditable(false);
            }
        }

        appMap.initMap();

        getBinder().forField(assetPosition).bind(AssetDto::getAssetPositionDto, AssetDto::setAssetPositionDto);
        formLayout.add(assetPosition);

        formLayout.add(longitude);
        formLayout.add(latitude);

        DatePicker guaranteeDate = VaadinComponents.getDatePicker(getDto().getQuaranteeDate());
        guaranteeDate.setLabel(Transl.get("Guarantee date"));
        getBinder().forField(guaranteeDate).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(AssetDto::getQuaranteeDate, AssetDto::setQuaranteeDate);
        formLayout.add(guaranteeDate);

        Checkbox depreciation = new Checkbox(Transl.get("Depreciation"));
        getBinder().forField(depreciation).bind(AssetDto::getDepreciation, AssetDto::setDepreciation);
        formLayout.add(depreciation);

        DatePicker removalDate = VaadinComponents.getDatePicker(getDto().getRemovalDate());
        removalDate.setLabel(Transl.get("Discarding"));
        getBinder().forField(removalDate).bind(AssetDto::getRemovalDate, AssetDto::setRemovalDate);
        formLayout.add(removalDate);

        ComboBox<EnumerationDto> category = new ComboBox<>(Transl.get("Category"));
        category.setItems(assetTypeList);
        category.setItemLabelGenerator(EnumerationDto::getName);
        getBinder().forField(category).bind(AssetDto::getCategory, AssetDto::setCategory);
        formLayout.add(category);

        AppMoneyField price = new AppMoneyField(Transl.get("Price"));
        getBinder().forField(price).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(AssetDto::getPrice, AssetDto::setPrice);

        TextField priceCategory = new TextField(Transl.get("Price category"));
        if (price.getValue() != null && price.getValue().doubleValue() > 0) {
            priceCategory.setValue(PriceCategoryEnum.getCategory(price.getValue().doubleValue()).getTextValue());
        }
        priceCategory.setReadOnly(true);

        price.addValueChangeListener(e -> {
            if (e.getValue() != null && e.getValue().doubleValue() > 0) {
                priceCategory.setValue(String.valueOf(PriceCategoryEnum.getCategory(e.getValue().doubleValue())));
            }
        });
        formLayout.add(priceCategory);

        ComboBox<AppCurrency> currency = new ComboBox<>(Transl.get("Currency"));
        currency.setItems(AppCurrency.values());
        currency.setItemLabelGenerator(appCurrency1 -> appCurrency1.name().toUpperCase());
        if (getDto().getAppCurrency() == null) {
            getDto().setAppCurrency(AppCurrency.CZK);
        }
        getBinder().forField(currency).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(AssetDto::getAppCurrency, AssetDto::setAppCurrency);

        if (getDto().getId() == null) {
            formLayout.add(price);
        } else {
            currency.setReadOnly(true);
        }

        formLayout.add(currency);

        DatePicker inventoryDate = VaadinComponents.getDatePicker(getDto().getInventoryDate());
        inventoryDate.setLabel(Transl.get("Inventory date"));
        getBinder().forField(inventoryDate).bind(AssetDto::getInventoryDate, AssetDto::setInventoryDate);
        formLayout.add(inventoryDate);

        ComboBox<AssetStateEnum> state = new ComboBox<>(Transl.get("Asset state"));
        state.setItems(AssetStateEnum.values());
        state.setItemLabelGenerator(AssetStateEnum::translate);
        getBinder().forField(state).asRequired(TextValues.CANNOT_BE_EMPTY).bind(AssetDto::getState, AssetDto::setState);
        formLayout.add(state);

        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.setWidthFull();
        verticalLayout.setPadding(false);
        verticalLayout.setMargin(false);

        TextArea description = new TextArea(Transl.get("Description"));
        description.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        getBinder().forField(description).bind(AssetDto::getDescription, AssetDto::setDescription);
        description.setWidthFull();
        description.setHeight(CssVariables.DEFAULT_FIELD_HEIGHT.getValue());


        TextArea destination = new TextArea(Transl.get("Destination"));
        destination.setMaxLength(255);
        getBinder().forField(destination).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(AssetDto::getDestination, AssetDto::setDestination);
        destination.setWidthFull();
        destination.setHeight(CssVariables.DEFAULT_FIELD_HEIGHT.getValue());

        verticalLayout.add(appMap, description, destination);

        getBinder().setBean(getDto());

        if (getDto().getDepreciation() == null) {
            depreciation.setValue(false);
        }

        VerticalLayout mainLayout = new VerticalLayout();
        mainLayout.add(formLayout, verticalLayout);
        mainLayout.setHeightFull();

        if (isDialog) {
            mainLayout.setMargin(false);
            mainLayout.setPadding(false);
        }

        if (getDto().getId() != null) {
            if (getDto().getInventoryDate() != null) {
                buyDate.setMax(inventoryDate.getValue());
                removalDate.setMin(inventoryDate.getValue());
            } else {
                removalDate.setMin(buyDate.getValue());
            }
            inventoryDate.setMin(buyDate.getValue());
        }

        if (readOnly) {
            name.setReadOnly(true);
            serialNumber.setReadOnly(true);
            employee.setReadOnly(true);
            category.setReadOnly(true);
            price.setReadOnly(true);
            buyDate.setReadOnly(true);
            guaranteeDate.setReadOnly(true);
            inventoryDate.setReadOnly(true);
            removalDate.setReadOnly(true);
            latitude.setReadOnly(true);
            longitude.setReadOnly(true);
            assetPosition.setReadOnly(true);
            description.setReadOnly(true);
            currency.setReadOnly(true);
            pohodaId.setReadOnly(true);
            id.setReadOnly(true);
            subjectComboBox.setReadOnly(true);
            type.setReadOnly(true);
            procurement.setReadOnly(true);
            depreciation.setReadOnly(true);
            state.setReadOnly(true);
            destination.setReadOnly(true);
            location.setReadOnly(true);
        }

        inventoryDate.addValueChangeListener(e -> {
            buyDate.setMax(e.getValue());
            removalDate.setMin(e.getValue());
        });
        buyDate.addValueChangeListener(e -> inventoryDate.setMin(e.getValue()));

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);
        this.add(mainLayout);
    }

    private String getEmployeeName(EmployeeDto employeeDto) {
        return assetComponentOperation.getEmployeeName(employeeDto);
    }
}
