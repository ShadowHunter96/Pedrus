package cz.bbn.cerberus.opportunity.ui.component.tabs;

import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.areatechnologysign.ui.AreaTechnologySignsBadgeComponent;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.field.AppMoneyField;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.component.ui.slider.AppSlider;
import cz.bbn.cerberus.commons.component.ui.tab.TabDtoComponent;
import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.custompermission.ui.CustomPermissionSingleListener;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.opportunity.OpportunityComponentOperation;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.opportunity.dto.OpportunityState;
import cz.bbn.cerberus.opportunity.ui.component.OpportunityNewDialog;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import org.apache.commons.lang3.StringUtils;

import java.math.BigDecimal;
import java.util.Objects;

public class OpportunityDetailTab extends TabDtoComponent<OpportunityDto> {

    private final OpportunityComponentOperation opportunityComponentOperation;
    private final boolean isDialog;
    private final boolean readOnly;
    private final AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent;
    private final ListService listService;

    public OpportunityDetailTab(OpportunityDto dto,
                                AppEnv appEnv, OpportunityComponentOperation opportunityComponentOperation,
                                CustomPermissionSingleListener listener, OpportunityNewDialog opportunityNewDialog,
                                boolean isDialog, boolean readOnly,
                                AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent,
                                ListService listService) {
        super(dto, opportunityComponentOperation.getSaveAction(listener, opportunityNewDialog), appEnv);
        this.opportunityComponentOperation = opportunityComponentOperation;
        this.isDialog = isDialog;
        this.readOnly = readOnly;
        this.areaTechnologySignsBadgeComponent = areaTechnologySignsBadgeComponent;
        this.listService = listService;
        initTab();
    }

    @Override
    protected void initTab() {
        removeAll();

        this.setId(RobotFrameworkVariables.OPPORTUNITY_DETAIL_CARD_ID.getValue());
        this.setSizeFull();
        setMargin(false);
        setPadding(false);

        FormLayout formLayout = new FormLayout();

        TextField id = new TextField(Transl.get("Id"));
        id.setEnabled(false);
        getBinder().forField(id).bind(OpportunityDto::getId, OpportunityDto::setId);
        if (StringUtils.isNoneEmpty(getDto().getId())) {
            formLayout.add(id);
        }

        TextField name = new TextField(Transl.get("Opportunity name"));
        name.setMaxLength(100);
        getBinder().forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(OpportunityDto::getName, OpportunityDto::setName);
        formLayout.add(name);

        ComboBox<SubjectDto> subject = new ComboBox<>(Transl.get("Customer"));
        subject.setItems(listService.getSubjectDtoListByCustomer());
        getBinder().forField(subject).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(OpportunityDto::getSubject, OpportunityDto::setSubject);
        subject.setItemLabelGenerator(SubjectDto::getName);
        formLayout.add(subject);

        ComboBox<SubjectDto> primarySupplier = new ComboBox<>(Transl.get("Primary supplier"));
        primarySupplier.setItems(listService.getSubjectDtoListByOwnCompanyOrSupplier());
        getBinder().forField(primarySupplier)
                .bind(OpportunityDto::getPrimarySupplier, OpportunityDto::setPrimarySupplier);
        primarySupplier.setItemLabelGenerator(SubjectDto::getName);
        formLayout.add(primarySupplier);

        AppMoneyField expectedReturn = new AppMoneyField(Transl.get("Expected return"));
        AppMoneyField expectedCosts = new AppMoneyField(Transl.get("Expected costs"));
        AppMoneyField volume = new AppMoneyField(Transl.get("Volume"));
        volume.addValueChangeListener(event -> {
            BigDecimal actualVolume = event.getValue() == null ? BigDecimal.ZERO : event.getValue();
            BigDecimal actualCosts = expectedCosts.getValue() == null ? BigDecimal.ZERO : expectedCosts.getValue();
            expectedReturn.setValue(actualVolume.subtract(actualCosts));
        });
        getBinder().forField(volume).bind(OpportunityDto::getVolume, OpportunityDto::setVolume);
        formLayout.add(volume);

        expectedCosts.addValueChangeListener(event -> {
            BigDecimal actualVolume = volume.getValue() == null ? BigDecimal.ZERO : volume.getValue();
            BigDecimal actualCosts = event.getValue() == null ? BigDecimal.ZERO : event.getValue();
            expectedReturn.setValue(actualVolume.subtract(actualCosts));
        });
        getBinder().forField(expectedCosts).bind(OpportunityDto::getExpectedCosts, OpportunityDto::setExpectedCosts);

        getBinder().forField(expectedReturn).bind(OpportunityDto::getExpectedReturn, OpportunityDto::setExpectedReturn);
        expectedReturn.setReadOnly(true);

        if (SecurityUtils.hasCustomPermission(DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue(),
                getDto().getId(), Permission.OPPORTUNITY_COSTS_RETURN_FIELDS.name())) {
            formLayout.add(expectedCosts, expectedReturn);
        }

        ComboBox<AppCurrency> currency = new ComboBox<>(Transl.get("Currency"));
        currency.setItems(AppCurrency.values());
        currency.setItemLabelGenerator(appCurrency1 -> appCurrency1.name().toUpperCase());
        getBinder().forField(currency)
                .bind(OpportunityDto::getAppCurrency, OpportunityDto::setAppCurrency);
        if (getDto().getAppCurrency() == null) {
            getDto().setAppCurrency(AppCurrency.CZK);
        }
        formLayout.add(currency);

        DatePicker guaranteeDate = VaadinComponents.getDatePicker(
                Transl.get("Realization date"), getDto().getStartDate());
        guaranteeDate.setLabel(Transl.get("Realization date"));
        getBinder().forField(guaranteeDate)
                .bind(OpportunityDto::getStartDate, OpportunityDto::setStartDate);
        formLayout.add(guaranteeDate);

        DatePicker dateOfFulfilment = VaadinComponents.getDatePicker(
                Transl.get("Date of fulfilment"), getDto().getDateOfFulfilment());
        getBinder().forField(dateOfFulfilment)
                .bind(OpportunityDto::getDateOfFulfilment, OpportunityDto::setDateOfFulfilment);
        formLayout.add(dateOfFulfilment);

        ComboBox<OpportunityState> state = new ComboBox<>(Transl.get("State"));
        state.setItems(OpportunityState.getAll());
        state.setItemLabelGenerator(opportunityState -> Transl.get(opportunityState.name().toLowerCase()));
        getBinder().forField(state).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(OpportunityDto::getState, OpportunityDto::setState);
        formLayout.add(state);

        AppSlider progress = new AppSlider(Transl.get("Progress"), getDto().getProgress(), "%");
        progress.setStep(25);
        getBinder().forField(progress.getPaperSlider()).bind(OpportunityDto::getProgress, OpportunityDto::setProgress);
        progress.getPaperSlider().addValueChangeListener(event ->
                progress.getElement().setAttribute("title", getProgressTitle(event.getValue())));
        progress.getElement().setAttribute("title", getProgressTitle(getDto().getProgress()));
        formLayout.add(progress);

        AppSlider successChance = new AppSlider(Transl.get("Success chance"), getDto().getSuccessChance(), "%");
        getBinder().forField(successChance.getPaperSlider())
                .bind(OpportunityDto::getSuccessChance, OpportunityDto::setSuccessChance);
        successChance.getPaperSlider().addValueChangeListener(event ->
                successChance.getElement().setAttribute("title", getSuccessChanceTitle(event.getValue())));
        successChance.getElement().setAttribute("title", getSuccessChanceTitle(getDto().getSuccessChance()));
        successChance.setStep(25);
        formLayout.add(successChance);

        ComboBox<SubjectDto> winningSubject = new ComboBox<>(Transl.get("Winning subject"));
        winningSubject.setItems(listService.getSubjectDtoList());
        getBinder().forField(winningSubject).bind(OpportunityDto::getWinnerSubject, OpportunityDto::setWinnerSubject);
        winningSubject.setItemLabelGenerator(SubjectDto::getName);
        formLayout.add(winningSubject);

        TextField winningTechnology = new TextField(Transl.get("Winning technology"));
        winningTechnology.setMaxLength(255);
        getBinder().forField(winningTechnology)
                .bind(OpportunityDto::getWinningTechnology, OpportunityDto::setWinningTechnology);
        formLayout.add(winningTechnology);

        ComboBox<UserDto> owner = new ComboBox<>(Transl.get("Owner"));
        owner.setItemLabelGenerator(UserDto::getName);
        owner.setItems(opportunityComponentOperation.getUserList());
        getBinder().forField(owner).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(OpportunityDto::getUser, OpportunityDto::setUser);
        formLayout.add(owner);

        if (getBinder().getBean().getId() != null && !SecurityUtils.hasPermission(Permission.CHANGE_OWNER)
                && !Objects.equals(getBinder().getBean().getUser().getId(), SecurityUtils.getCurrentUserId())) {
            owner.setReadOnly(true);
        }


        HorizontalLayout descriptionLayout = new HorizontalLayout();
        descriptionLayout.setWidthFull();

        TextArea description = new TextArea(Transl.get("Description"));
        description.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        getBinder().forField(description)
                .bind(OpportunityDto::getDescription, OpportunityDto::setDescription);
        description.setWidthFull();
        description.setHeight(CssVariables.DEFAULT_FIELD_HEIGHT.getValue());

        descriptionLayout.add(description);

        getBinder().setBean(getDto());

        VerticalLayout mainLayout = new VerticalLayout();
        mainLayout.add(formLayout);

        if (areaTechnologySignsBadgeComponent != null) {
            mainLayout.add(areaTechnologySignsBadgeComponent);
        }

        mainLayout.add(descriptionLayout);
        mainLayout.setHeightFull();
        if (isDialog) {
            mainLayout.setMargin(false);
            mainLayout.setPadding(false);
        }

        if (readOnly) {
            name.setReadOnly(true);
            subject.setReadOnly(true);
            owner.setReadOnly(true);
            description.setReadOnly(true);
            currency.setReadOnly(true);
            primarySupplier.setReadOnly(true);
            volume.setReadOnly(true);
            dateOfFulfilment.setReadOnly(true);
            guaranteeDate.setReadOnly(true);
            state.setReadOnly(true);
            progress.setEnabled(false);
            successChance.setEnabled(false);
            winningSubject.setReadOnly(true);
            winningTechnology.setReadOnly(true);
        }

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);
        this.add(mainLayout);
    }

    private String getProgressTitle(int progress) {
        switch (progress) {
            case 0:
                return Transl.get("Identified");
            case 25:
                return Transl.get("In progress");
            case 50:
                return Transl.get("Work on offer");
            case 75:
                return Transl.get("Put forward");
            case 100:
                return Transl.get("Decided");
            default:
                return "";
        }
    }

    private String getSuccessChanceTitle(int progress) {
        switch (progress) {
            case 0:
                return Transl.get("Prospectus");
            case 25:
                return Transl.get("Identified");
            case 50:
                return Transl.get("Offer entered");
            case 75:
                return Transl.get("Offer put forward");
            case 100:
                return Transl.get("Win");
            default:
                return "";
        }
    }

    @Override
    public void loadTab() {
        areaTechnologySignsBadgeComponent.loadData();
    }
}
