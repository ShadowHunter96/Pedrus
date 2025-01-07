package cz.bbn.cerberus.subject.ui.component.tab;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.html.Anchor;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.adis.AdisReliable;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.field.AppMoneyField;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.component.ui.tab.TabDtoComponent;
import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.commons.validator.KeyValidator;
import cz.bbn.cerberus.commons.validator.MinMaxValidator;
import cz.bbn.cerberus.custompermission.ui.CustomPermissionSingleListener;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.SubjectComponentOperation;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.subject.ui.component.SubjectNewDialog;
import cz.bbn.cerberus.suppliertype.dto.SupplierTypeDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.Objects;

public class SubjectDetailTab extends TabDtoComponent<SubjectDto> {

    private final AppEnv appEnv;
    private final SubjectDto dto;
    private final List<UserDto> userList;
    private final boolean isDialog;
    private final boolean readOnly;
    private final List<SupplierTypeDto> supplierTypeList;
    private final boolean ownCompanyAlreadyExists;
    private final String ownCompanyName;

    public SubjectDetailTab(SubjectDto dto, SubjectComponentOperation subjectComponentOperation,
                            AppEnv appEnv, SubjectNewDialog subjectNewDialog,
                            CustomPermissionSingleListener customPermissionSingleListener, boolean isDialog,
                            boolean readOnly, List<SupplierTypeDto> supplierTypeList, boolean ownCompanyAlreadyExists,
                            String ownCompanyName) {
        super(dto, subjectComponentOperation.getSaveAction(customPermissionSingleListener, dto, subjectNewDialog),
                appEnv);
        this.appEnv = appEnv;
        this.dto = dto;
        this.userList = subjectComponentOperation.getUserList();
        this.isDialog = isDialog;
        this.readOnly = readOnly;
        this.supplierTypeList = supplierTypeList;
        this.ownCompanyAlreadyExists = ownCompanyAlreadyExists;
        this.ownCompanyName = ownCompanyName;
        initTab();
    }

    @Override
    protected void initTab() {
        setName("subject-detail");
        removeAll();
        setHeightFull();
        setPadding(false);
        setMargin(false);

        FormLayout formLayout = new FormLayout();
        formLayout.setWidthFull();

        HorizontalLayout context = new HorizontalLayout();
        context.setSizeFull();
        VerticalLayout rightSite = new VerticalLayout();
        rightSite.setWidthFull();
        rightSite.setMargin(false);
        Binder<SubjectDto> binder = getBinder();


        TextField id = new TextField(Transl.get("Subject shortcut"));
        id.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        id.setMaxLength(20);

        TextField name = new TextField(Transl.get("Name"));
        name.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        name.setMaxLength(100);

        boolean isLocalSubject = Boolean.TRUE.equals(dto.getLocalSubject());

        TextField ico = new TextField(Transl.get("ICO"));
        ico.setMaxLength(40);
        ico.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        ico.setReadOnly(isLocalSubject);
        if (dto.getId() != null) {
            ico.setReadOnly(true);
        }

        TextField companyName = new TextField(Transl.get("Company name"));
        companyName.setMaxLength(100);
        companyName.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        companyName.setReadOnly(isLocalSubject);

        TextField court = new TextField(Transl.get("Court"));
        court.setMaxLength(100);
        court.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        court.setReadOnly(isLocalSubject);

        TextField fileNumber = new TextField(Transl.get("File number"));
        fileNumber.setMaxLength(100);
        fileNumber.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        fileNumber.setReadOnly(isLocalSubject);

        TextField register = new TextField(Transl.get("Register"));
        register.setMaxLength(100);
        register.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        register.setReadOnly(isLocalSubject);

        TextArea lawForm = new TextArea(Transl.get("Legal form"));
        lawForm.setWidth(CssVariables.SMALL_TEXT_AREA_WIDTH.getValue());
        lawForm.setHeight(CssVariables.SMALL_TEXT_AREA_HEIGHT.getValue());
        lawForm.setMaxLength(100);
        lawForm.setReadOnly(isLocalSubject);

        TextArea address = new TextArea(Transl.get("Address"));
        address.setWidth(CssVariables.SMALL_TEXT_AREA_WIDTH.getValue());
        address.setHeight(CssVariables.SMALL_TEXT_AREA_HEIGHT.getValue());
        address.setMaxLength(200);
        address.setReadOnly(isLocalSubject);

        TextField enlistDate = new TextField(Transl.get("Registration date"));
        enlistDate.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        enlistDate.setReadOnly(isLocalSubject);
        enlistDate.setMaxLength(20);

        AppMoneyField startingCapital = new AppMoneyField(Transl.get("Starting capital"));
        startingCapital.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        startingCapital.setReadOnly(isLocalSubject);

        ComboBox<AppCurrency> currency = new ComboBox<>(Transl.get("Currency"));
        currency.setItems(AppCurrency.values());
        currency.setItemLabelGenerator(appCurrency1 -> appCurrency1.name().toUpperCase());
        if (dto.getAppCurrency() == null) {
            dto.setAppCurrency(AppCurrency.CZK);
        }
        currency.setReadOnly(isLocalSubject);

        if (isLocalSubject) {
            startingCapital.setValue(dto.getCapitalDecimal());
            startingCapital.setTitle(dto.getCapital());
        } else {
            binder.forField(startingCapital).bind(SubjectDto::getCapitalDecimal, SubjectDto::setCapitalDecimal);
        }

        TextArea companions = new TextArea(Transl.get("Companions"));
        companions.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        companions.setWidth(CssVariables.SMALL_TEXT_AREA_WIDTH.getValue());
        companions.setHeight(CssVariables.SMALL_TEXT_AREA_HEIGHT.getValue());
        companions.setReadOnly(isLocalSubject);

        TextField dic = new TextField(Transl.get("DIC"));
        dic.setMaxLength(20);
        dic.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        dic.setReadOnly(isLocalSubject);

        ComboBox<AdisReliable> reliable = new ComboBox<>(Transl.get("Reliable"));
        reliable.setItems(AdisReliable.values());
        reliable.setItemLabelGenerator(e -> Transl.get(e.name()));
        reliable.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        reliable.setReadOnly(isLocalSubject);

        TextField unreliableFrom = new TextField(Transl.get("Unreliable from"));
        unreliableFrom.setMaxLength(20);
        unreliableFrom.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        unreliableFrom.setReadOnly(isLocalSubject);

        TextArea standardAccount = new TextArea(Transl.get("Standard account"));
        standardAccount.setWidth(CssVariables.SMALL_TEXT_AREA_WIDTH.getValue());
        standardAccount.setHeight(CssVariables.SMALL_TEXT_AREA_HEIGHT.getValue());
        standardAccount.setReadOnly(isLocalSubject);

        TextArea nonStandardAccount = new TextArea(Transl.get("Non standard account"));
        nonStandardAccount.setWidth(CssVariables.SMALL_TEXT_AREA_WIDTH.getValue());
        nonStandardAccount.setHeight(CssVariables.SMALL_TEXT_AREA_HEIGHT.getValue());
        nonStandardAccount.setReadOnly(isLocalSubject);

        TextArea description = new TextArea(Transl.get("Description"));
        description.setWidthFull();
        description.setHeight(CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());
        description.setMaxLength(255);

        Button urlButton = VaadinComponents.getEyeButton();
        HorizontalLayout urlLayout = new HorizontalLayout();
        urlLayout.setAlignItems(FlexComponent.Alignment.END);
        TextField url = new TextField(Transl.get("Customer URL"));
        url.addValueChangeListener(event -> {
            if (StringUtils.isNoneEmpty(event.getValue())) {
                urlButton.removeClassName("disabled-color");
                urlButton.setEnabled(true);
            } else {
                urlButton.addClassName("disabled-color");
                urlButton.setEnabled(false);
            }
        });
        url.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        url.setMaxLength(100);
        urlLayout.add(url);

        urlButton.setTabIndex(-1);
        if (StringUtils.isEmpty(url.getValue())) {
            urlButton.addClassName("disabled-color");
            urlButton.setEnabled(false);
        }
        urlButton.addClickListener(buttonClickEvent -> {
            if (url.getValue() != null && StringUtils.isNoneEmpty(url.getValue())) {
                if (url.getValue().contains("http")) {
                    getUI().ifPresent(ui -> ui.getPage().open(url.getValue()));
                } else {
                    String tempUrl = "http://".concat(url.getValue());
                    getUI().ifPresent(ui -> ui.getPage().open(tempUrl));
                }
            } else {
                getUI().ifPresent(ui -> ui.getPage().open(""));
            }
        });
        urlLayout.add(urlButton);

        ComboBox<UserDto> user = new ComboBox<>(Transl.get("Owner"));
        user.setItemLabelGenerator(UserDto::getName);
        user.setItems(userList);
        user.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());

        if (dto.getId() != null && !SecurityUtils.hasPermission(Permission.CHANGE_OWNER)
                && !Objects.equals(dto.getUserDto().getId(), SecurityUtils.getCurrentUserId())) {
            user.setReadOnly(true);
        }

        binder.forField(id).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .withValidator(new KeyValidator()).withValidator(new MinMaxValidator(2, 20))
                .bind(SubjectDto::getId, SubjectDto::setId);

        binder.forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(SubjectDto::getName, SubjectDto::setName);

        Checkbox customer = new Checkbox(Transl.get("Is customer"));

        ComboBox<SupplierTypeDto> supplierType = new ComboBox<>(Transl.get("Supplier type"));
        supplierType.setReadOnly(Boolean.FALSE != dto.getSupplier());
        supplierType.setItemLabelGenerator(SupplierTypeDto::getName);
        supplierType.setItems(supplierTypeList);

        Checkbox supplier = new Checkbox(Transl.get("Is supplier"));
        supplier.addValueChangeListener(event -> {
            supplierType.setReadOnly(!event.getValue());
            if (Boolean.FALSE.equals(event.getValue())) {
                supplierType.setValue(null);
            }
        });

        supplierType.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());

        Checkbox ownCompany = new Checkbox(Transl.get("Is own company"));

        if (!Boolean.TRUE.equals(dto.getOwnCompany()) && ownCompanyAlreadyExists) {
            ownCompany.setReadOnly(true);
            ownCompany.addClickListener(e -> ErrorNotification.show(Transl.get(
                    ErrorCode.OWN_COMPANY_ALREADY_EXISTS.getError(), ownCompanyName), appEnv)
            );
        }

        binder.forField(ico).bind(SubjectDto::getIco, SubjectDto::setIco);
        binder.forField(companyName).bind(SubjectDto::getCompanyName, SubjectDto::setCompanyName);
        binder.forField(court).bind(SubjectDto::getCourt, SubjectDto::setCourt);
        binder.forField(fileNumber).bind(SubjectDto::getFileNumber, SubjectDto::setFileNumber);
        binder.forField(register).bind(SubjectDto::getRegister, SubjectDto::setRegister);
        binder.forField(lawForm).bind(SubjectDto::getLawForm, SubjectDto::setLawForm);
        binder.forField(address).bind(SubjectDto::getAddress, SubjectDto::setAddress);
        binder.forField(enlistDate).bind(SubjectDto::getEnlistDate, SubjectDto::setEnlistDate);
        binder.forField(companions).bind(SubjectDto::getCompanions, SubjectDto::setCompanions);
        binder.forField(dic).bind(SubjectDto::getDic, SubjectDto::setDic);
        binder.forField(reliable).bind(SubjectDto::getReliable, SubjectDto::setReliable);
        binder.forField(unreliableFrom).bind(SubjectDto::getUnreliableFrom, SubjectDto::setUnreliableFrom);
        binder.forField(standardAccount).bind(SubjectDto::getStandardAccount, SubjectDto::setStandardAccount);
        binder.forField(nonStandardAccount).bind(SubjectDto::getNonStandardAccount, SubjectDto::setNonStandardAccount);
        binder.forField(user).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(SubjectDto::getUserDto, SubjectDto::setUserDto);
        binder.forField(customer).bind(SubjectDto::getCustomer, SubjectDto::setCustomer);
        binder.forField(supplier).bind(SubjectDto::getSupplier, SubjectDto::setSupplier);
        binder.forField(supplierType).bind(SubjectDto::getSupplierType, SubjectDto::setSupplierType);
        binder.forField(ownCompany).bind(SubjectDto::getOwnCompany, SubjectDto::setOwnCompany);
        binder.forField(currency).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(SubjectDto::getAppCurrency, SubjectDto::setAppCurrency);

        binder.forField(description)
                .bind(SubjectDto::getDescription, SubjectDto::setDescription);

        binder.forField(url)
                .bind(SubjectDto::getUrl, SubjectDto::setUrl);

        if (getBinder().getBean().getId() == null) {
            user.setValue(SecurityUtils.getCurrentUserDto());
        }

        if (dto.getId() != null) {
            id.setReadOnly(true);
        }

        if ((dto.getUnreliableFrom() != null && !"".equals(dto.getUnreliableFrom())) ||
                (dto.getReliable() != null && dto.getReliable().equals(AdisReliable.NO))) {
            reliable.setInvalid(true);
            unreliableFrom.setInvalid(true);
        }

        formLayout.add(id, name, ico, dic, companyName, reliable, unreliableFrom, court, fileNumber, register,
                enlistDate, startingCapital, currency, urlLayout, user, customer, supplier, ownCompany, supplierType);
        if (dto.getId() != null) {
            Anchor justiceLink = new Anchor(Transl.get("Justice link"));
            justiceLink.setHref(appEnv.getJusticeUrlBeginning() + getBinder().getBean().getIco()
                    + appEnv.getJusticeUrlEnd());
            Span justiceSpan = new Span(Transl.get("Justice link"));
            justiceSpan.addClassName("link-cursor");
            justiceSpan.addClickListener(e -> getUI().ifPresent(ui -> ui.getPage().open(justiceLink.getHref())));
            formLayout.add(justiceSpan);
        }

        FormLayout secondFormLayout = new FormLayout();
        secondFormLayout.setWidthFull();

        secondFormLayout.add(lawForm, address, companions, standardAccount, nonStandardAccount);

        VerticalLayout verticalLayout = new VerticalLayout();

        if (readOnly) {
            id.setReadOnly(true);
            name.setReadOnly(true);
            ico.setReadOnly(true);
            companyName.setReadOnly(true);
            court.setReadOnly(true);
            fileNumber.setReadOnly(true);
            register.setReadOnly(true);
            lawForm.setReadOnly(true);
            address.setReadOnly(true);
            enlistDate.setReadOnly(true);
            startingCapital.setReadOnly(true);
            companions.setReadOnly(true);
            dic.setReadOnly(true);
            reliable.setReadOnly(true);
            unreliableFrom.setReadOnly(true);
            standardAccount.setReadOnly(true);
            nonStandardAccount.setReadOnly(true);
            user.setReadOnly(true);
            description.setReadOnly(true);
            url.setReadOnly(true);
            currency.setReadOnly(true);
            customer.setReadOnly(true);
            supplier.setReadOnly(true);
            ownCompany.setReadOnly(true);
            supplierType.setReadOnly(true);

        }

        verticalLayout.add(formLayout, secondFormLayout, description);
        verticalLayout.setHeightFull();
        add(verticalLayout);

        if (isDialog) {
            verticalLayout.setMargin(false);
            verticalLayout.setPadding(false);
        }

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);
        LayoutUtils.setBigInfiniteColumnResponsiveSteps(secondFormLayout);
    }

    @Override
    public void loadTab() {
        initTab();
    }
}
