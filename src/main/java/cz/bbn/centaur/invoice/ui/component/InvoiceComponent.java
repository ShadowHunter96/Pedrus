package cz.bbn.cerberus.invoice.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.html.H3;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.binder.ValidationResult;
import cz.bbn.cerberus.areatechnologysign.AreaTechnologySignDto;
import cz.bbn.cerberus.areatechnologysign.ui.AreaTechnologySignsMultiselect;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.field.AppMoneyField;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.WarningNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.contract.ui.ContractSalesDetailView;
import cz.bbn.cerberus.dph.dto.DphDto;
import cz.bbn.cerberus.invoice.InvoiceComponentOperation;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.invoice.dto.InvoiceState;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.ui.SubjectDetailView;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.math.BigDecimal;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Optional;

@Slf4j
public class InvoiceComponent extends VerticalLayout {

    private final InvoiceComponentOperation invoiceComponentOperation;
    private final InvoiceDto invoiceDto;
    private final Binder<InvoiceDto> binder = new Binder<>();
    private final boolean readOnly;
    private final List<UserDto> userList;
    private final AppEnv appEnv;

    private final Checkbox transferProtocol = new Checkbox(Transl.get("Acceptance/transfer protocol"));
    private final Checkbox expenditureInvoice = new Checkbox(Transl.get("Expenditure invoice"));
    private final Checkbox ordered = new Checkbox(Transl.get("Ordered"));
    private final List<AreaTechnologySignDto> areaTechnologySignDtoList;

    private InvoiceInvoicedComponent invoiceInvoicedComponent;
    private TextArea description;

    public InvoiceComponent(InvoiceDto invoiceDto, InvoiceComponentOperation invoiceComponentOperation,
                            boolean readOnly, List<UserDto> userList, AppEnv appEnv,
                            List<AreaTechnologySignDto> areaTechnologySignDtoList) {
        this.invoiceComponentOperation = invoiceComponentOperation;
        this.invoiceDto = invoiceDto;
        this.readOnly = readOnly;
        this.userList = userList;
        this.appEnv = appEnv;
        this.areaTechnologySignDtoList = areaTechnologySignDtoList;
        init();
    }

    private void init() {

        H3 firstTitle = new H3(Transl.get("Planned invoice"));
        firstTitle.getElement().getStyle().set("margin-top", "0em");

        FormLayout firstFormLayout = new FormLayout();
        firstFormLayout.setSizeFull();
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(firstFormLayout);

        ContractDto contractDto = invoiceComponentOperation.getContract(invoiceDto.getContractDto().getId());

        binder.setBean(invoiceDto);

        TextField stringId = new TextField(Transl.get("Id"));
        stringId.setReadOnly(true);
        binder.forField(stringId).bind(InvoiceDto::getStringId, InvoiceDto::setStringId);

        DatePicker invoicingDate = VaadinComponents.getDatePicker(invoiceDto.getInvoicingDate());
        invoicingDate.setLabel(Transl.get("Invoicing planned date"));
        binder.forField(invoicingDate).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(InvoiceDto::getInvoicingDate, InvoiceDto::setInvoicingDate);
        invoicingDate.setLocale(Locale.forLanguageTag(Transl.DEFAULT_LANG.toLowerCase()));

        HorizontalLayout subjectLayout = new HorizontalLayout();
        subjectLayout.setAlignItems(Alignment.END);
        Button subjectButton = VaadinComponents.getEyeButton();
        TextField subject = new TextField(Transl.get("Customer"));
        if (contractDto != null && contractDto.getSubjectDto() != null) {
            subject.setValue(contractDto.getSubjectDto().getName());
        }
        subject.setReadOnly(true);
        subjectLayout.add(subject);

        subjectButton.setTabIndex(-1);
        if (StringUtils.isEmpty(subject.getValue())) {
            subjectButton.addClassName("disabled-color");
            subjectButton.setEnabled(false);
        }
        subjectButton.addClickListener(e -> {
            if (contractDto != null && contractDto.getSubjectDto() != null) {
                UI.getCurrent().navigate(SubjectDetailView.ROUTE + "/" + contractDto.getSubjectDto().getId());
            } else {
                WarningNotification.show(Transl.get("No subject available"), appEnv);
            }
        });
        subjectLayout.add(subjectButton);

        HorizontalLayout contractLayout = new HorizontalLayout();
        contractLayout.setAlignItems(Alignment.END);
        TextField contract = new TextField(Transl.get("Contract"));
        if (contractDto != null) {
            contract.setValue(contractDto.getName());
        }
        contract.setReadOnly(true);
        contractLayout.add(contract);

        Button contractButton = VaadinComponents.getEyeButton();
        contractButton.setTabIndex(-1);
        contractButton.addClickListener(e -> {
            if (contractDto != null) {
                UI.getCurrent().navigate(ContractSalesDetailView.ROUTE + "/" + contractDto.getId());
            } else {
                WarningNotification.show(Transl.get("No contract available"), appEnv);
            }
        });
        contractLayout.add(contractButton);

        binder.forField(transferProtocol).bind(InvoiceDto::getTransferProtocol, InvoiceDto::setTransferProtocol);

        description = new TextArea(Transl.get("Description"));
        description.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        description.setWidthFull();
        description.setMaxHeight("25em");
        description.setMinHeight("9em");
        binder.forField(description).asRequired((itemDto, valueContext) -> {
                    if (binder.getBean().getState() == InvoiceState.NEW &&
                            (description.getValue() == null || "".equals(description.getValue().trim()))) {
                        return ValidationResult.error(Transl.get(TextValues.CANNOT_BE_EMPTY));
                    }
                    return ValidationResult.ok();
                })
                .bind(InvoiceDto::getDescription, InvoiceDto::setDescription);

        AppMoneyField priceNoVat = new AppMoneyField();
        priceNoVat.setLabel(Transl.get("Price without VAT"));
        binder.forField(priceNoVat).asRequired((itemDto, valueContext) -> {
                    if (priceNoVat.getValue() == null || priceNoVat.getValue().doubleValue() <= 0D) {
                        return ValidationResult.error(Transl.get(TextValues.CANNOT_BE_EMPTY));
                    }
                    return ValidationResult.ok();
                })
                .bind(InvoiceDto::getPriceNoVatBigDecimal, InvoiceDto::setPriceNoVatBigDecimal);

        Optional<DphDto> dph = invoiceComponentOperation.getDphList().stream().filter(
                dphDto -> Boolean.TRUE.equals(dphDto.getDefaultValue())).findAny();
        if (invoiceDto.getDphDto() == null && dph.isPresent()) {
            invoiceDto.setDphDto(dph.get());
        }

        ComboBox<DphDto> vat = new ComboBox<>(Transl.get("VAT"));
        List<DphDto> rates = invoiceComponentOperation.getDphList();
        vat.setItems(rates);
        vat.setItemLabelGenerator(DphDto::getName);
        binder.forField(vat).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(InvoiceDto::getDphDto, InvoiceDto::setDphDto);

        Double fullPrice = 0D;
        if (invoiceDto.getDphDto() != null) {
            fullPrice = invoiceComponentOperation.getFullPrice(invoiceDto.getPriceNoVat(), invoiceDto.getDphDto()
                    .getValue());
        } else if (invoiceDto.getPriceNoVat() != null) {
            fullPrice = invoiceDto.getPriceNoVat();
        }

        AppMoneyField fullPriceField = new AppMoneyField();
        fullPriceField.setLabel(Transl.get("Full price"));
        fullPriceField.setValue(BigDecimal.valueOf(fullPrice));
        fullPriceField.setReadOnly(true);

        ComboBox<AppCurrency> currency = new ComboBox<>(Transl.get("Currency"));
        currency.setItems(AppCurrency.values());
        currency.setItemLabelGenerator(appCurrency1 -> appCurrency1.name().toUpperCase());
        if (invoiceDto.getAppCurrency() == null) {
            invoiceDto.setAppCurrency(AppCurrency.CZK);
        }
        binder.forField(currency).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(InvoiceDto::getAppCurrency, InvoiceDto::setAppCurrency);

        vat.addValueChangeListener(e -> fullPriceField.setValue(
                BigDecimal.valueOf(invoiceComponentOperation.getFullPriceValue(priceNoVat, vat))));
        priceNoVat.addValueChangeListener(e -> fullPriceField.setValue(
                BigDecimal.valueOf(invoiceComponentOperation.getFullPriceValue(priceNoVat, vat))));

        IntegerField daysToPay = new IntegerField(Transl.get("Days to pay"));
        daysToPay.setMin(0);
        binder.forField(daysToPay).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(InvoiceDto::getDaysToPay, InvoiceDto::setDaysToPay);

        TextArea addressee = new TextArea(Transl.get("Addressee"));
        addressee.setMaxLength(255);
        addressee.setWidthFull();
        addressee.setMaxHeight("25em");
        addressee.setMinHeight("9em");
        binder.forField(addressee).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY)).
                bind(InvoiceDto::getAddressee, InvoiceDto::setAddressee);

        TextArea expenseInvoices = new TextArea(Transl.get("Expense invoices"));
        expenseInvoices.setMaxLength(255);
        expenseInvoices.setMaxHeight("25em");
        expenseInvoices.setMinHeight("9em");
        binder.forField(expenseInvoices).bind(InvoiceDto::getExpenseInvoices, InvoiceDto::setExpenseInvoices);

        IntegerField reminder = new IntegerField(Transl.get("Reminder in working days"));
        reminder.setMin(0);
        reminder.setMax(60);
        binder.forField(reminder).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(InvoiceDto::getReminderWorkDays, InvoiceDto::setReminderWorkDays);

        TextField evidenceNo = new TextField(Transl.get("Evidence customer number"));
        evidenceNo.setReadOnly(true);
        evidenceNo.setValue(invoiceDto.getContractDto().getEvidenceNo());

        TextField contractNumber = new TextField(Transl.get("Contract number"));
        contractNumber.setReadOnly(true);
        contractNumber.setValue(invoiceDto.getContractDto().getId());


        ComboBox<UserDto> owner = new ComboBox<>(Transl.get("Owner"));
        owner.setItems(userList);
        owner.setItemLabelGenerator(UserDto::getName);
        binder.forField(owner).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(InvoiceDto::getUserDto, InvoiceDto::setUserDto);

        if (getBinder().getBean().getId() != null && !SecurityUtils.hasPermission(Permission.CHANGE_OWNER)
                && !Objects.equals(getBinder().getBean().getUserDto().getId(), SecurityUtils.getCurrentUserId())) {
            owner.setReadOnly(true);
        }

        ComboBox<InvoiceState> state = new ComboBox<>(Transl.get("State"));
        state.setItems(InvoiceState.values());
        state.setItemLabelGenerator(InvoiceState::getTranslatedName);
        binder.forField(state).bind(InvoiceDto::getState, InvoiceDto::setState);
        state.setReadOnly(true);

        firstFormLayout.add(stringId, invoicingDate, subjectLayout, contractLayout, transferProtocol, priceNoVat, vat,
                fullPriceField, currency, daysToPay, reminder, evidenceNo, contractNumber, owner, state);

        MultiSelectComboBox<AreaTechnologySignDto> areaTechnologySign =
                new AreaTechnologySignsMultiselect(
                        areaTechnologySignDtoList);
        binder.forField(areaTechnologySign)
                .bind(InvoiceDto::getAreaTechnologySignDtoSet, InvoiceDto::setAreaTechnologySignDtoSet);

        FormLayout secondFormLayout = new FormLayout();
        secondFormLayout.add(description, addressee, expenseInvoices, areaTechnologySign);
        secondFormLayout.setWidthFull();

        H3 secondTitle = new H3(Transl.get("Invoicing done"));

        invoiceInvoicedComponent = new InvoiceInvoicedComponent(invoiceDto, binder, true);

        add(firstTitle, firstFormLayout,
                secondFormLayout, secondTitle, invoiceInvoicedComponent);

        priceNoVat.addValueChangeListener(e -> fullPriceField.setValue(BigDecimal.valueOf(
                changeFullPrice(vat.getValue(), priceNoVat.getValue().doubleValue()))));
        vat.addValueChangeListener(e -> fullPriceField.setValue(BigDecimal.valueOf(
                changeFullPrice(vat.getValue(), priceNoVat.getValue().doubleValue()))));

        daysToPay.addValueChangeListener(e -> invoiceInvoicedComponent.changeDueDate(daysToPay.getValue()));

        if (readOnly) {
            invoicingDate.setReadOnly(true);
            subject.setReadOnly(true);
            contract.setReadOnly(true);
            transferProtocol.setReadOnly(true);
            priceNoVat.setReadOnly(true);
            vat.setReadOnly(true);
            fullPriceField.setReadOnly(true);
            daysToPay.setReadOnly(true);
            reminder.setReadOnly(true);
            description.setReadOnly(true);
            addressee.setReadOnly(true);
            expenditureInvoice.setReadOnly(true);
            ordered.setReadOnly(true);
            currency.setReadOnly(true);
            owner.setReadOnly(true);
            expenseInvoices.setReadOnly(true);
            areaTechnologySign.setReadOnly(true);
        }
    }

    public Binder<InvoiceDto> getBinder() {
        InvoiceDto dto = binder.getBean();
        dto.setTransferProtocol(transferProtocol.getValue());
        binder.setBean(dto);
        return binder;
    }

    public void changeDueDate(Integer daysToPay) {
        invoiceInvoicedComponent.changeDueDate(daysToPay);
    }

    public void setDescriptionRequired(boolean required) {
        description.setRequired(required);
    }

    private Double changeFullPrice(DphDto vatRate, Double priceNoVat) {
        Double fullPrice = 0D;
        if (vatRate != null && priceNoVat != null) {
            fullPrice = invoiceComponentOperation.getFullPrice(priceNoVat, vatRate.getValue());
        } else if (priceNoVat != null) {
            fullPrice = priceNoVat;
        }
        return fullPrice;
    }
}
