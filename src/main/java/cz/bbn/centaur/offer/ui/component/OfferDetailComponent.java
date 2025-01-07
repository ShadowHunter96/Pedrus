package cz.bbn.cerberus.offer.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.areatechnologysign.ui.AreaTechnologySignsBadgeComponent;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.field.AppMoneyField;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.offer.dto.OfferDto;
import cz.bbn.cerberus.offer.dto.OfferState;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class OfferDetailComponent extends VerticalLayout {

    private final OfferDto dto;
    private final Binder<OfferDto> binder;
    private final List<UserDto> userList;
    private final List<SubjectDto> ownOrganizationSubjectList;
    private final AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent;
    private final boolean readOnly;
    private final List<OpportunityDto> opportunityList;
    private final List<SubjectDto> customerList;

    public OfferDetailComponent(OfferDto dto, Binder<OfferDto> binder, List<OpportunityDto> opportunityList,
                                List<UserDto> userList, List<SubjectDto> ownOrganizationSubjectList,
                                AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent,
                                boolean readOnly, List<SubjectDto> customerList) {
        this.dto = dto;
        this.binder = binder;
        this.areaTechnologySignsBadgeComponent = areaTechnologySignsBadgeComponent;
        this.userList = userList;
        this.ownOrganizationSubjectList = ownOrganizationSubjectList;
        this.readOnly = readOnly;
        this.opportunityList = opportunityList;
        this.customerList = customerList;
        initComponent();
    }

    public OfferDetailComponent(OfferDto dto, Binder<OfferDto> binder, List<OpportunityDto> opportunityList,
                                List<UserDto> userList, List<SubjectDto> ownOrganizationSubjectList,
                                boolean readOnly, List<SubjectDto> customerList) {
        this.dto = dto;
        this.binder = binder;
        this.areaTechnologySignsBadgeComponent = null;
        this.userList = userList;
        this.ownOrganizationSubjectList = ownOrganizationSubjectList;
        this.opportunityList = opportunityList;
        this.readOnly = readOnly;
        this.customerList = customerList;
        initComponent();
    }

    private void initComponent() {
        FormLayout formLayout = new FormLayout();

        TextField id = new TextField(Transl.get("Id"));

        id.setValue(String.valueOf(dto.getId()));
        id.setReadOnly(true);
        binder.forField(id).bind(OfferDto::getId, OfferDto::setId);
        if (dto.getId() != null) {
            formLayout.add(id);
        }

        TextField name = new TextField(Transl.get("Name"));
        name.setMaxLength(255);
        binder.forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(OfferDto::getName, OfferDto::setName);
        formLayout.add(name);

        ComboBox<SubjectDto> subject = new ComboBox<>(Transl.get("Customer"));
        subject.setItems(customerList);
        subject.setItemLabelGenerator(SubjectDto::getName);
        if (dto.getSubjectDto() != null) {
            subject.setValue(dto.getSubjectDto());
        }
        binder.forField(subject).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(OfferDto::getSubjectDto, OfferDto::setSubjectDto);

        if (dto.getOpportunityDto() != null) {
            subject.setValue(dto.getOpportunityDto().getSubject());
        }
        if (dto.getId() != null) {
            subject.setReadOnly(true);
        }

        formLayout.add(subject);

        ComboBox<OpportunityDto> opportunityComboBox = new ComboBox<>(Transl.get("Opportunity:"));
        if (opportunityList != null) {
            opportunityComboBox.setItems(opportunityList);
            opportunityComboBox.setItemLabelGenerator(OpportunityDto::getName);
            binder.forField(opportunityComboBox).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(OfferDto::getOpportunityDto, OfferDto::setOpportunityDto);
            formLayout.add(opportunityComboBox);
            subject.addValueChangeListener(e -> {
                if (e.getValue() == null) {
                    opportunityComboBox.setItems(opportunityList);
                } else {
                    opportunityComboBox.setItems(getOppBySub(e.getValue().getId()));
                }
            });
            if (dto.getOpportunityDto() != null && dto.getOpportunityDto().getSubject() != null) {
                opportunityComboBox.setItems(getOppBySub(dto.getOpportunityDto().getSubject().getId()));
                opportunityComboBox.setValue(dto.getOpportunityDto());
            }
        }

        AppMoneyField priceWithoutVat = new AppMoneyField(Transl.get("Price without VAT"));
        binder.forField(priceWithoutVat).bind(OfferDto::getPriceWithoutVat, OfferDto::setPriceWithoutVat);
        formLayout.add(priceWithoutVat);

        ComboBox<AppCurrency> currency = new ComboBox<>(Transl.get("Currency"));
        currency.setItems(AppCurrency.values());
        currency.setItemLabelGenerator(appCurrency1 -> appCurrency1.name().toUpperCase());
        binder.forField(currency)
                .bind(OfferDto::getAppCurrency, OfferDto::setAppCurrency);
        if (dto.getAppCurrency() == null) {
            dto.setAppCurrency(AppCurrency.CZK);
        }
        formLayout.add(currency);

        DatePicker offerDate = VaadinComponents.getDatePicker(Transl.get("Offer date"), dto.getOfferDate());
        binder.forField(offerDate)
                .bind(OfferDto::getOfferDate, OfferDto::setOfferDate);
        formLayout.add(offerDate);

        DatePicker validityDate = VaadinComponents.getDatePicker(Transl.get("Offer validity"), dto.getValidityDate());
        binder.forField(validityDate).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(OfferDto::getValidityDate, OfferDto::setValidityDate);
        formLayout.add(validityDate);

        offerDate.addValueChangeListener(e -> {
            if (validityDate.getValue() == null && offerDate.getValue() != null) {
                validityDate.setValue(offerDate.getValue().plusDays(30));
            }
        });

        TextField customerReference = new TextField(Transl.get("Customer reference number"));
        customerReference.setMaxLength(100);
        binder.forField(customerReference)
                .bind(OfferDto::getCustomerReference, OfferDto::setCustomerReference);
        formLayout.add(customerReference);

        ComboBox<UserDto> processedByUser = new ComboBox<>(Transl.get("Processed by user"));
        processedByUser.setItems(userList);
        processedByUser.setItemLabelGenerator(UserDto::getName);
        processedByUser.setValue(SecurityUtils.getCurrentUserDto());
        binder.forField(processedByUser)
                .asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(OfferDto::getProcessedByUserDto, OfferDto::setProcessedByUserDto);

        if (dto.getId() != null && !SecurityUtils.hasPermission(Permission.CHANGE_OWNER)
                && !Objects.equals(dto.getProcessedByUserDto().getId(), SecurityUtils.getCurrentUserId())) {
            processedByUser.setReadOnly(true);
        }

        formLayout.add(processedByUser);

        ComboBox<SubjectDto> ownOrganization = new ComboBox<>(Transl.get("Own organization"));
        ComboBox.ItemFilter<SubjectDto> subjectFilter = (item, filterText) ->
                StringUtils.stripAccents(item.getName().toLowerCase())
                        .contains(StringUtils.stripAccents(filterText.toLowerCase()));

        ownOrganization.setItems(subjectFilter, ownOrganizationSubjectList);
        binder.forField(ownOrganization)
                .bind(OfferDto::getOwnOrganizationSubjectDto, OfferDto::setOwnOrganizationSubjectDto);
        ownOrganization.setItemLabelGenerator(SubjectDto::getName);
        formLayout.add(ownOrganization);

        Checkbox assurance = new Checkbox(Transl.get("Assurance"));
        binder.forField(assurance).bind(OfferDto::getAssurance, OfferDto::setAssurance);
        formLayout.add(assurance);


        AppMoneyField priceAssurance = new AppMoneyField(Transl.get("Price assurance"));
        binder.forField(priceAssurance).bind(OfferDto::getPriceAssurance, OfferDto::setPriceAssurance);
        formLayout.add(priceAssurance);

        Checkbox sent = new Checkbox(Transl.get("Sent"));
        binder.forField(sent).bind(OfferDto::getSent, OfferDto::setSent);
        formLayout.add(sent);

        Checkbox explanation = new Checkbox(Transl.get("Explanation"));
        binder.forField(explanation).bind(OfferDto::getExplanation, OfferDto::setExplanation);
        formLayout.add(explanation);

        ComboBox<OfferState> state = new ComboBox<>(Transl.get("State"));
        state.setItems(OfferState.values());
        state.setItemLabelGenerator(offerState -> Transl.get(offerState.name().toLowerCase()));
        binder.forField(state).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(OfferDto::getState, OfferDto::setState);
        formLayout.add(state);

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);
        this.add(formLayout);

        if (areaTechnologySignsBadgeComponent != null) {
            this.add(areaTechnologySignsBadgeComponent);
        }

        HorizontalLayout marketLayout = new HorizontalLayout();
        marketLayout.setAlignItems(FlexComponent.Alignment.END);
        marketLayout.setPadding(false);
        marketLayout.setMargin(false);
        marketLayout.setWidthFull();

        Button marketEye = VaadinComponents.getEyeButton();
        TextField marketUrl = new TextField(Transl.get("Market url"));
        marketUrl.setWidthFull();
        marketUrl.addValueChangeListener(event -> {
            if (StringUtils.isNoneEmpty(event.getValue())) {
                marketEye.removeClassName("disabled-color");
                marketEye.setEnabled(true);
            } else {
                marketEye.addClassName("disabled-color");
                marketEye.setEnabled(false);
            }
        });
        binder.forField(marketUrl).bind(OfferDto::getMarketUrl, OfferDto::setMarketUrl);
        marketLayout.add(marketUrl);

        if (StringUtils.isEmpty(marketUrl.getValue())) {
            marketEye.addClassName("disabled-color");
            marketEye.setEnabled(false);
        }
        marketEye.addClickListener(buttonClickEvent ->
                UI.getCurrent().getPage().open(marketUrl.getValue(), "_blank"));
        if (dto.getId() != null) {
            marketLayout.add(marketEye);
        }

        HorizontalLayout svnLayout = new HorizontalLayout();
        svnLayout.setAlignItems(FlexComponent.Alignment.END);
        Button svnEye = VaadinComponents.getEyeButton();
        TextField svnUrl = new TextField(Transl.get("Offer SVN URL"));
        svnUrl.addValueChangeListener(event -> {
            if (StringUtils.isNoneEmpty(event.getValue())) {
                svnEye.removeClassName("disabled-color");
                svnEye.setEnabled(true);
            } else {
                svnEye.addClassName("disabled-color");
                svnEye.setEnabled(false);
            }
        });
        binder.forField(svnUrl).bind(OfferDto::getSvnUrl, OfferDto::setSvnUrl);
        svnUrl.setWidthFull();
        svnLayout.add(svnUrl);
        svnLayout.setWidthFull();

        svnEye.addClickListener(buttonClickEvent ->
                UI.getCurrent().getPage().open(svnUrl.getValue(), "_blank"));

        if (StringUtils.isEmpty(svnUrl.getValue())) {
            svnEye.addClassName("disabled-color");
            svnEye.setEnabled(false);
        }
        if (dto.getId() != null) {
            svnLayout.add(svnEye);
        }
        HorizontalLayout urlLayout = new HorizontalLayout();
        urlLayout.setPadding(false);
        urlLayout.setMargin(false);
        urlLayout.add(marketLayout, svnLayout);
        urlLayout.setWidthFull();
        this.add(urlLayout);

        if (readOnly) {
            id.setReadOnly(true);
            name.setReadOnly(true);
            subject.setReadOnly(true);
            priceWithoutVat.setReadOnly(true);
            currency.setReadOnly(true);
            offerDate.setReadOnly(true);
            validityDate.setReadOnly(true);
            customerReference.setReadOnly(true);
            processedByUser.setReadOnly(true);
            ownOrganization.setReadOnly(true);
            assurance.setReadOnly(true);
            priceAssurance.setReadOnly(true);
            sent.setReadOnly(true);
            explanation.setReadOnly(true);
            state.setReadOnly(true);
            marketUrl.setReadOnly(true);
            svnUrl.setReadOnly(true);
        }


        binder.setBean(dto);

    }

    private List<OpportunityDto> getOppBySub(String id) {
        List<OpportunityDto> oppList = new ArrayList<>();
        for (OpportunityDto opportunityDto : opportunityList) {
            if (opportunityDto.getSubject() != null && opportunityDto.getSubject().getId().equals(id)) {
                oppList.add(opportunityDto);
            }
        }
        return oppList;
    }
}
