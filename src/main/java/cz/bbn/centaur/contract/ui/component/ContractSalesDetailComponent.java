package cz.bbn.cerberus.contract.ui.component;

import com.vaadin.componentfactory.TooltipAlignment;
import com.vaadin.componentfactory.TooltipPosition;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.binder.ValidationResult;
import cz.bbn.cerberus.areatechnologysign.ui.AreaTechnologySignsBadgeComponent;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppTooltip;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.field.AppMoneyField;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.WarningNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.ContractComponentOperation;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.contract.dto.ContractEndingDays;
import cz.bbn.cerberus.contract.dto.ContractInternalType;
import cz.bbn.cerberus.contract.dto.Signed;
import cz.bbn.cerberus.contract.ui.ContractSalesDetailView;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public class ContractSalesDetailComponent extends VerticalLayout {

    private final ContractDto dto;
    private final List<ContractDto> contractList;
    private final List<UserDto> userList;
    private final List<ContractTypeDto> contractTypeList;
    private final AppEnv appEnv;
    private final Binder<ContractDto> binder;
    private final boolean isDialog;
    private final boolean readOnly;
    private final AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent;
    private final ListService listService;
    private final ContractInternalType internalType;
    private final ContractComponentOperation componentOperation;

    private ComboBox<ContractDto> connectedContract;

    public ContractSalesDetailComponent(ContractDto dto, List<ContractDto> contractList, List<UserDto> userList,
                                        AppEnv appEnv, Binder<ContractDto> binder, boolean isDialog, boolean readOnly,
                                        AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent,
                                        List<ContractTypeDto> contractTypeList, ListService listService,
                                        ComboBox<ContractDto> connectedContract, ContractInternalType internalType,
                                        ContractComponentOperation componentOperation) {
        dto.setInternalType(internalType);
        this.dto = dto;
        this.contractList = contractList;
        this.userList = userList;
        this.appEnv = appEnv;
        this.binder = binder;
        this.isDialog = isDialog;
        this.readOnly = readOnly;
        this.contractTypeList = contractTypeList;
        this.areaTechnologySignsBadgeComponent = areaTechnologySignsBadgeComponent;
        this.listService = listService;
        this.connectedContract = connectedContract;
        this.internalType = internalType;
        this.componentOperation = componentOperation;
        initComponent();
    }

    private void initComponent() {
        setMargin(false);
        setPadding(false);
        FormLayout formLayout = new FormLayout();

        ComboBox<OpportunityDto> opportunity = new ComboBox<>(Transl.get("Opportunity"));

        ComboBox<SubjectDto> subject = new ComboBox<>(Transl.get("Customer"));
        ComboBox<SubjectDto> contractParty = new ComboBox<>(Transl.get("Provider"));

        ComboBox.ItemFilter<SubjectDto> subjectFilter = (item, filterText) ->
                StringUtils.stripAccents(item.getName().toLowerCase())
                        .contains(StringUtils.stripAccents(filterText.toLowerCase()));

        if (internalType == ContractInternalType.SALES) {
            formLayout.add(subject);

            subject.setItems(subjectFilter, componentOperation.getOurCompCustomerList());
            binder.forField(subject).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(ContractDto::getSubjectDto, ContractDto::setSubjectDto);
            subject.setItemLabelGenerator(SubjectDto::getName);
            subject.addValueChangeListener(event ->
                    opportunity.setItems(listService.getOpportunityDtoListByCustomer(event.getValue().getId())));

            contractParty.setItems(componentOperation.getSupplierOwnCompList());
            contractParty.setItemLabelGenerator(SubjectDto::getName);
            binder.forField(contractParty).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(ContractDto::getContractParty, ContractDto::setContractParty);
            contractParty.setReadOnly(dto.getId() != null);
        } else {
            formLayout.add(contractParty);

            contractParty.setItems(componentOperation.getSupplierList());
            contractParty.setItemLabelGenerator(SubjectDto::getName);
            binder.forField(contractParty).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(ContractDto::getContractParty, ContractDto::setContractParty);
            contractParty.setReadOnly(dto.getId() != null);

            List<SubjectDto> ourCompanies = componentOperation.getOurCompanyList();
            subject.setItems(ourCompanies);
            binder.forField(subject).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(ContractDto::getSubjectDto, ContractDto::setSubjectDto);
            subject.setItemLabelGenerator(SubjectDto::getName);
            subject.setValue(componentOperation.getOurCompanyValue(ourCompanies));
        }

        ComboBox<ContractTypeDto> contractType = new ComboBox<>(Transl.get("Contract type"));
        contractType.setItems(contractTypeList);
        contractType.setItemLabelGenerator(ContractTypeDto::getName);
        binder.forField(contractType).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ContractDto::getType, ContractDto::setType);
        formLayout.add(contractType);

        if (internalType == ContractInternalType.SALES) {
            formLayout.add(contractParty);
        } else {
            formLayout.add(subject);
        }

        AppTooltip contractTooltip = new AppTooltip();
        contractTooltip.setAlignment(TooltipAlignment.BOTTOM);
        contractTooltip.setPosition(TooltipPosition.BOTTOM);
        this.add(contractTooltip);

        HorizontalLayout contractLayout = new HorizontalLayout();
        contractLayout.setAlignItems(FlexComponent.Alignment.END);
        connectedContract = new ComboBox<>(Transl.get("Connected contract"));
        connectedContract.setItems(filterContractBySubject(subject.getValue()));
        binder.forField(connectedContract).bind(ContractDto::getConnectedContract, ContractDto::setConnectedContract);
        connectedContract.setItemLabelGenerator(ContractDto::getName);
        contractLayout.add(connectedContract);

        Button viewButton = VaadinComponents.getEyeButton();
        connectedContract.addValueChangeListener(e -> {
            setTooltipText(contractTooltip, e.getValue());
            if (e.getValue() != null) {
                viewButton.removeClassName("disabled-color");
                viewButton.setEnabled(true);
            } else {
                viewButton.addClassName("disabled-color");
                viewButton.setEnabled(false);
            }
        });
        setTooltipText(contractTooltip, getFullContract(dto.getConnectedContract()));

        subject.addValueChangeListener(e -> connectedContract.setItems(filterContractBySubject(e.getValue())));

        viewButton.setTabIndex(-1);
        if (connectedContract.getValue() == null) {
            viewButton.addClassName("disabled-color");
            viewButton.setEnabled(false);
        }
        viewButton.addClickListener(buttonClickEvent -> {
            if (connectedContract.getValue() != null) {
                UI.getCurrent().navigate(ContractSalesDetailView.ROUTE + "/" + connectedContract.getValue().getId());
            } else {
                WarningNotification.show(Transl.get("No contract is selected"), appEnv);
            }

        });
        contractLayout.add(viewButton);
        contractTooltip.attachToComponent(viewButton);
        formLayout.add(contractLayout);

        TextField name = new TextField(Transl.get("Name"));
        name.setMaxLength(100);
        binder.forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ContractDto::getName, ContractDto::setName);
        formLayout.add(name);

        DatePicker effectStart = VaadinComponents.getDatePicker(null);
        DatePicker endTime = VaadinComponents.getDatePicker(dto.getEndContract());
        DatePicker validityStart = VaadinComponents.getDatePicker(null);
        validityStart.setLabel(Transl.get("Validity start"));
        binder.forField(validityStart).withValidator((date, valueContext) -> {
                    if (date != null && effectStart.getValue() != null
                            && date.isAfter(effectStart.getValue())) {
                        return ValidationResult.error(
                                Transl.get("Validity start have to be smaller or equal than effective date"));
                    }
                    if (date != null && endTime.getValue() != null
                            && date.isAfter(endTime.getValue())) {
                        return ValidationResult.error(
                                Transl.get("Validity start have to be smaller or equal than end contract"));
                    }
                    return ValidationResult.ok();
                })
                .bind(ContractDto::getValidityStart, ContractDto::setValidityStart);
        formLayout.add(validityStart);

        effectStart.setLabel(Transl.get("Effective date"));
        binder.forField(effectStart).bind(ContractDto::getEffectStart, ContractDto::setEffectStart);
        formLayout.add(effectStart);

        endTime.setLabel(Transl.get("End contract"));
        binder.forField(endTime).withValidator((date, valueContext) -> {
            if (date != null && effectStart.getValue() != null &&
                    (date.isBefore(effectStart.getValue()) || date.equals(effectStart.getValue()))) {
                return ValidationResult.error(Transl.get("End contract have to be greater than effective date"));
            }

            if (date != null && validityStart.getValue() != null &&
                    (date.isBefore(validityStart.getValue()) || date.equals(validityStart.getValue()))) {
                return ValidationResult.error(Transl.get("End contract have to be greater than validity start"));
            }
            return ValidationResult.ok();
        }).bind(ContractDto::getEndContract, ContractDto::setEndContract);
        formLayout.add(endTime);

        ComboBox<ContractEndingDays> contractEndingDays = new ComboBox<>(Transl.get("End contract notify(days)"));
        contractEndingDays.setItems(ContractEndingDays.values());
        contractEndingDays.setItemLabelGenerator(contractEndingDays1 -> String.valueOf(contractEndingDays1.getDays()));
        binder.forField(contractEndingDays).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ContractDto::getSendNotificationDaysBefore, ContractDto::setSendNotificationDaysBefore);
        formLayout.add(contractEndingDays);

        ComboBox<Boolean> additions = new ComboBox<>(Transl.get("Additions"));
        additions.setItems(Arrays.asList(true, false));
        additions.setItemLabelGenerator(this::getBooleanString);
        binder.forField(additions).bind(ContractDto::getAddition, ContractDto::setAddition);
        formLayout.add(additions);

        String evidenceNoTitle = dto.getId() == null || contractType.getValue() == null
                ? Transl.get("Ev. n. counterparty")
                : Transl.get("Ev. n. {0} counterparty", contractType.getValue().getName());
        TextField evidenceNo = new TextField(evidenceNoTitle);
        evidenceNo.setMaxLength(100);
        binder.forField(evidenceNo).bind(ContractDto::getEvidenceNo, ContractDto::setEvidenceNo);
        formLayout.add(evidenceNo);

        ComboBox<UserDto> owner = new ComboBox<>(Transl.get("Owner"));
        owner.setItemLabelGenerator(UserDto::getName);
        owner.setItems(userList);
        binder.forField(owner).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ContractDto::getUserDto, ContractDto::setUserDto);

        if (dto.getId() != null && !SecurityUtils.hasPermission(Permission.CHANGE_OWNER)
                && !Objects.equals(dto.getUserDto().getId(), SecurityUtils.getCurrentUserId())) {
            owner.setReadOnly(true);
        }

        formLayout.add(owner);

        TextArea description = new TextArea(Transl.get("Description"));
        //description.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        binder.forField(description).bind(ContractDto::getDescription, ContractDto::setDescription);
        description.setWidthFull();
        description.setHeight(CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());

        if (dto.getId() != null) {
            TextField id = new TextField(Transl.get("Contract number"));
            id.setReadOnly(true);
            id.setValue(dto.getId());
            formLayout.add(id);
        }

        ComboBox<EnumerationDto> contractState = new ComboBox<>(Transl.get("Contract state"));
        contractState.setItemLabelGenerator(EnumerationDto::getName);
        contractState.setItems(listService.getEnumerationDtoList("CONTRACT_STATE", dto.getId() != null));
        binder.forField(contractState).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ContractDto::getContractState, ContractDto::setContractState);
        formLayout.add(contractState);

        AppMoneyField priceNoVat = new AppMoneyField(Transl.get("Price without VAT"));
        binder.forField(priceNoVat).bind(ContractDto::getPriceNoVat, ContractDto::setPriceNoVat);
        formLayout.add(priceNoVat);

        ComboBox<AppCurrency> currency = new ComboBox<>(Transl.get("Currency"));
        currency.setItems(AppCurrency.values());
        currency.setItemLabelGenerator(appCurrency1 -> appCurrency1.name().toUpperCase());
        if (dto.getAppCurrency() == null) {
            dto.setAppCurrency(AppCurrency.CZK);
        }
        binder.forField(currency).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ContractDto::getAppCurrency, ContractDto::setAppCurrency);
        formLayout.add(currency);

        IntegerField maturityInvoice = new IntegerField(Transl.get("Maturity invoice"));
        maturityInvoice.setMin(0);
        binder.forField(maturityInvoice).bind(ContractDto::getMaturityInvoice, ContractDto::setMaturityInvoice);
        formLayout.add(maturityInvoice);

        if (subject.getValue() != null) {
            opportunity.setItems(listService.getOpportunityDtoListByCustomer(subject.getValue().getId()));
        }
        opportunity.setItemLabelGenerator(OpportunityDto::getName);
        binder.forField(opportunity)
                .bind(ContractDto::getOpportunityDto, ContractDto::setOpportunityDto);
        formLayout.add(opportunity);

        ComboBox<Signed> signed = new ComboBox<>(Transl.get("Signed"));
        signed.setItemLabelGenerator(signedActual -> Transl.get(signedActual.name()));
        signed.setItems(Signed.values());
        binder.forField(signed).bind(ContractDto::getSigned, ContractDto::setSigned);
        formLayout.add(signed);

        TextField type = new TextField(Transl.get("Internal type"));
        type.setReadOnly(true);
        type.setValue(Transl.get(dto.getInternalType().name() + " contract"));
        formLayout.add(type);

        binder.setBean(dto);

        VerticalLayout mainLayout = new VerticalLayout();
        mainLayout.add(formLayout);

        TextField contractSubject = new TextField(Transl.get("Contract subject"));
        contractSubject.setMaxLength(255);
        contractSubject.setWidthFull();
        binder.forField(contractSubject).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ContractDto::getContractSubject, ContractDto::setContractSubject);
        mainLayout.add(contractSubject);

        if (areaTechnologySignsBadgeComponent != null) {
            mainLayout.add(areaTechnologySignsBadgeComponent);
        }

        mainLayout.add(description);

        mainLayout.setHeightFull();

        if (isDialog) {
            mainLayout.setPadding(false);
            mainLayout.setMargin(false);
        }

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);

        binder.setBean(dto);

        this.add(mainLayout);

        if (dto.getId() == null && dto.getUserDto() == null) {
            owner.setValue(SecurityUtils.getCurrentUserDto());
        }

        if (dto.getId() != null) {
            contractType.setReadOnly(true);
            connectedContract.setReadOnly(
                    dto.getType() != null && Boolean.TRUE.equals(dto.getType().getConnectionRequired()));
        }

        contractType.addValueChangeListener(e -> {
            if (e.getValue() != null && Boolean.TRUE.equals(e.getValue().getConnectionRequired())) {
                connectedContract.setRequired(true);
                connectedContract.setErrorMessage(Transl.get(TextValues.CANNOT_BE_EMPTY));
            } else {
                connectedContract.setRequired(false);
            }
        });

        if (readOnly) {
            name.setReadOnly(true);
            subject.setReadOnly(true);
            endTime.setReadOnly(true);
            owner.setReadOnly(true);
            description.setReadOnly(true);
            connectedContract.setReadOnly(true);
            subject.setReadOnly(true);
            validityStart.setReadOnly(true);
            effectStart.setReadOnly(true);
            additions.setReadOnly(true);
            contractParty.setReadOnly(true);
            contractType.setReadOnly(true);
            currency.setReadOnly(true);
        }
    }

    private String getBooleanString(Boolean value) {
        if (Boolean.TRUE.equals(value)) {
            return Transl.get("yes");
        }
        if (Boolean.FALSE.equals(value)) {
            return Transl.get("no");
        }
        return "";
    }

    private List<ContractDto> filterContractBySubject(SubjectDto subjectDto) {
        List<ContractDto> contractBySubjectList = new ArrayList<>();
        if (subjectDto != null) {
            for (ContractDto contract : contractList) {
                if (contract.getSubjectDto() != null && contract.getSubjectDto().getId().equals(subjectDto.getId())
                        && contract.getInternalType() == ContractInternalType.SALES) {
                    contractBySubjectList.add(contract);
                }
            }
        }
        return contractBySubjectList;
    }

    private void setTooltipText(AppTooltip appTooltip, ContractDto contractDto) {
        appTooltip.removeAll();
        VerticalLayout verticalLayout = new VerticalLayout();
        if (contractDto != null) {
            verticalLayout.add(new Label(Transl.get("Name") + ": " + contractDto.getName()));
            if (contractDto.getEffectStart() != null) {
                verticalLayout.add(new Label(Transl.get("Effective date") + ": " +
                        AppUtils.formatDate(contractDto.getEffectStart())));
            }
            if (contractDto.getEndContract() != null) {
                verticalLayout.add(new Label(Transl.get("End contract") + ": " +
                        AppUtils.formatDate(contractDto.getEndContract())));
            }
            verticalLayout.add(new Label(Transl.get("Contract number") + ": " + contractDto.getId()));
            if (contractDto.getUserDto() != null) {
                verticalLayout.add(new Label(Transl.get("Owner") + ": " + contractDto.getUserDto().getName()));
            }
            appTooltip.add(verticalLayout);
        } else {
            appTooltip.add(new Label(Transl.get("No contract selected")));
        }
    }

    private ContractDto getFullContract(ContractDto connectedContract) {
        if (connectedContract != null) {
            for (ContractDto contractDto : contractList) {
                if (contractDto.getId().equals(connectedContract.getId())) {
                    return contractDto;
                }
            }
        }
        return null;
    }
}
