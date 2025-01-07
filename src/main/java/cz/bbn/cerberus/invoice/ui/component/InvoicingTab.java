package cz.bbn.cerberus.invoice.ui.component;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.binder.ValidationResult;
import cz.bbn.cerberus.areatechnologysign.AreaTechnologyComponentOperation;
import cz.bbn.cerberus.areatechnologysign.ui.AreaTechnologySignsMultiselect;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.field.AppMoneyField;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enums.AppCurrency;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.dph.dto.DphDto;
import cz.bbn.cerberus.invoice.InvoiceComponentOperation;
import cz.bbn.cerberus.invoice.dto.InvoiceState;
import cz.bbn.cerberus.invoice.dto.InvoicingDto;
import cz.bbn.cerberus.invoice.dto.InvoicingPeriod;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Locale;

public class InvoicingTab extends TabSimpleComponent {

    private final ContractDto contractDto;
    private final SubjectDto subjectDto;
    private final List<ContractDto> contractDtoList;
    private final List<SubjectDto> subjectList;
    private final Binder<InvoicingDto> binder;
    private final InvoiceComponentOperation invoiceComponentOperation;
    private final boolean periodic;
    private final AreaTechnologyComponentOperation areaTechnologyComponentOperation;

    public InvoicingTab(SubjectDto subjectDto, ContractDto contractDto,
                        List<ContractDto> contractDtoList, List<SubjectDto> subjectList,
                        Binder<InvoicingDto> binder, InvoiceComponentOperation invoiceComponentOperation,
                        boolean periodic, AreaTechnologyComponentOperation areaTechnologyComponentOperation) {
        this.contractDto = contractDto;
        this.subjectDto = subjectDto;
        this.contractDtoList = contractDtoList;
        this.subjectList = subjectList;
        this.binder = binder;
        this.invoiceComponentOperation = invoiceComponentOperation;
        this.periodic = periodic;
        this.areaTechnologyComponentOperation = areaTechnologyComponentOperation;
        initTab();
    }

    private void initTab() {
        InvoicingDto invoicingDto = new InvoicingDto();
        invoicingDto.setReminderWorkDays(0);
        invoicingDto.setTransferProtocolPresent(false);

        AreaTechnologySignsMultiselect areaTechnologySign =
                new AreaTechnologySignsMultiselect();

        binder.setBean(invoicingDto);
        DphDto defaultValue = invoiceComponentOperation.getDefaultDph();
        if (defaultValue != null) {
            invoicingDto.setDphDto(defaultValue);
            invoicingDto.setToBeInvoicedState(false);
        }

        FormLayout firstFormLayout = new FormLayout();
        firstFormLayout.setSizeFull();

        ComboBox<InvoicingPeriod> invoicingPeriod = new ComboBox<>(Transl.get("Invoicing period"));
        invoicingPeriod.setItems(InvoicingPeriod.values());
        invoicingPeriod.setItemLabelGenerator(InvoicingPeriod::getText);

        DatePicker startDate = VaadinComponents.getDatePicker(null);
        startDate.setLabel(Transl.get("Start date"));
        startDate.setLocale(Locale.forLanguageTag(Transl.get("language-locale")));

        DatePicker endDate = VaadinComponents.getDatePicker(null);
        endDate.setLabel(Transl.get("End date"));
        endDate.setLocale(Locale.forLanguageTag(Transl.get("language-locale")));

        IntegerField invoicingDay = new IntegerField(Transl.get("Invoicing day"));

        DatePicker invoicingDate = VaadinComponents.getDatePicker(null);
        invoicingDate.setLabel(Transl.get("Invoicing day"));

        if (periodic) {
            binder.forField(invoicingPeriod).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(InvoicingDto::getInvoicingPeriod, InvoicingDto::setInvoicingPeriod);
            invoicingPeriod.setValue(InvoicingPeriod.valueOf("MONTHLY"));
            firstFormLayout.add(invoicingPeriod);

            binder.forField(startDate).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(InvoicingDto::getInvoicingStart, InvoicingDto::setInvoicingStart);
            firstFormLayout.add(startDate);

            binder.forField(endDate).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(InvoicingDto::getInvoicingEnd, InvoicingDto::setInvoicingEnd);
            firstFormLayout.add(endDate);

            startDate.setMax(endDate.getValue());
            endDate.setMin(startDate.getValue());

            startDate.addValueChangeListener(e -> endDate.setMin(e.getValue()));
            endDate.addValueChangeListener(e -> startDate.setMax(e.getValue()));

            binder.forField(invoicingDay).asRequired((itemDto, valueContext) -> {
                        if (invoicingDay.getValue() == null) {
                            return ValidationResult.error(Transl.get(TextValues.CANNOT_BE_EMPTY));
                        } else if (invoicingDay.getValue() < 1 || invoicingDay.getValue() > 31) {
                            return ValidationResult.error(Transl.get("Value must be between {0} and {1}",
                                    String.valueOf(1), String.valueOf(31)));
                        } else {
                            return ValidationResult.ok();
                        }
                    })
                    .bind(InvoicingDto::getInvoicingDay, InvoicingDto::setInvoicingDay);
            invoicingDay.setValue(LocalDate.now().getDayOfMonth());

            invoicingPeriod.addValueChangeListener(e -> invoicingDay.setValue(LocalDate.now().getDayOfMonth()));
            firstFormLayout.add(invoicingDay);
        } else {
            binder.forField(invoicingDate).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(InvoicingDto::getInvoicingStart, InvoicingDto::setInvoicingStart);
            firstFormLayout.add(invoicingDate);

            if (contractDto != null && contractDto.getEndContract() != null) {
                invoicingDate.setValue(contractDto.getEndContract());
            }
        }

        ComboBox<SubjectDto> subject = new ComboBox<>(Transl.get("Customer"));
        subject.setItems(subjectList);
        subject.setItemLabelGenerator(SubjectDto::getName);
        binder.forField(subject).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(InvoicingDto::getSubject, InvoicingDto::setSubject);
        firstFormLayout.add(subject);

        ComboBox<ContractDto> contract = new ComboBox<>(Transl.get("Contract"));
        contract.setItemLabelGenerator(ContractDto::getName);
        binder.forField(contract).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(InvoicingDto::getContract, InvoicingDto::setContract);
        firstFormLayout.add(contract);
        contract.addValueChangeListener(event -> {
            if (event.isFromClient()) {
                if (event.getValue() != null) {
                    subject.setValue(event.getValue().getSubjectDto());
                    areaTechnologySign.setAreaTechnologySignDtoList(
                            areaTechnologyComponentOperation.getAreaTechnologySignDtoList(ObjectType.CONTRACT,
                                    event.getValue().getId()));
                    areaTechnologySign.initComponent();
                } else {
                    subject.setValue(null);
                }
            }
        });

        subject.addValueChangeListener(event -> {
            if (event.isFromClient()) {
                if (event.getValue() != null) {
                    contract.setItems(contractDtoList.stream().filter(actualSubjectDto ->
                            actualSubjectDto.getSubjectDto().getId().equals(event.getValue().getId())).toList());
                } else {
                    contract.setItems(contractDtoList);
                }
                contract.setValue(null);
            }
        });

        Checkbox transferProtocol = new Checkbox(Transl.get("Transfer protocol"));
        binder.forField(transferProtocol)
                .bind(InvoicingDto::isTransferProtocolPresent, InvoicingDto::setTransferProtocolPresent);
        firstFormLayout.add(transferProtocol);

        AppMoneyField priceNoVat = new AppMoneyField();
        priceNoVat.setLabel(Transl.get("Price without VAT"));
        if (Boolean.TRUE.equals(periodic)) {
            binder.forField(priceNoVat)
                    .bind(InvoicingDto::getPriceNoVatBigDecimal, InvoicingDto::setPriceNoVatBigDecimal);
        } else {
            binder.forField(priceNoVat).asRequired((itemDto, valueContext) -> {
                        if (priceNoVat.getValue() == null || priceNoVat.getValue().doubleValue() <= 0D) {
                            return ValidationResult.error(Transl.get(TextValues.CANNOT_BE_EMPTY));
                        }
                        return ValidationResult.ok();
                    })
                    .bind(InvoicingDto::getPriceNoVatBigDecimal, InvoicingDto::setPriceNoVatBigDecimal);
        }
        firstFormLayout.add(priceNoVat);

        ComboBox<AppCurrency> currency = new ComboBox<>(Transl.get("Currency"));
        currency.setItems(AppCurrency.values());
        currency.setItemLabelGenerator(appCurrency1 -> appCurrency1.name().toUpperCase());
        if (invoicingDto.getAppCurrency() == null) {
            invoicingDto.setAppCurrency(AppCurrency.CZK);
        }
        binder.forField(currency).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(InvoicingDto::getAppCurrency, InvoicingDto::setAppCurrency);
        firstFormLayout.add(currency);

        ComboBox<DphDto> vat = new ComboBox<>(Transl.get("VAT"));
        List<DphDto> rates = invoiceComponentOperation.getDphList();
        vat.setItems(rates);
        vat.setItemLabelGenerator(DphDto::getName);
        binder.forField(vat).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(InvoicingDto::getDphDto, InvoicingDto::setDphDto);
        firstFormLayout.add(vat);

        AppMoneyField fullPriceField = new AppMoneyField();
        fullPriceField.setLabel(Transl.get("Full price"));
        fullPriceField.setValue(BigDecimal.valueOf(0D));
        fullPriceField.setReadOnly(true);
        firstFormLayout.add(fullPriceField);

        vat.addValueChangeListener(e -> fullPriceField.setValue(
                BigDecimal.valueOf(invoiceComponentOperation.getFullPriceValue(priceNoVat, vat))));
        priceNoVat.addValueChangeListener(e -> fullPriceField.setValue(
                BigDecimal.valueOf(invoiceComponentOperation.getFullPriceValue(priceNoVat, vat))));

        IntegerField maturityInvoice = new IntegerField(Transl.get("Days to pay"));
        maturityInvoice.setMin(0);
        binder.forField(maturityInvoice).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(InvoicingDto::getMaturityInvoice, InvoicingDto::setMaturityInvoice);
        firstFormLayout.add(maturityInvoice);

        IntegerField reminder = new IntegerField(Transl.get("Reminder"));
        reminder.setMin(0);
        reminder.setMax(60);
        binder.forField(reminder).bind(InvoicingDto::getReminderWorkDays, InvoicingDto::setReminderWorkDays);
        firstFormLayout.add(reminder);

        FormLayout secondFormLayout = new FormLayout();
        secondFormLayout.setSizeFull();

        TextArea description = new TextArea(Transl.get("Description"));
        description.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        description.setWidthFull();
        description.setMaxHeight("25em");
        description.setMinHeight("9em");
        binder.forField(description).bind(InvoicingDto::getDescription, InvoicingDto::setDescription);
        secondFormLayout.add(description);

        TextArea addressee = new TextArea(Transl.get("Addressee"));
        addressee.setMaxLength(255);
        addressee.setWidthFull();
        addressee.setMaxHeight("25em");
        addressee.setMinHeight("9em");
        binder.forField(addressee)
                .asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(InvoicingDto::getAddressee, InvoicingDto::setAddressee);
        secondFormLayout.add(addressee);

        binder.forField(areaTechnologySign).bind(InvoicingDto::getAreaTechnologySignDtoSet, InvoicingDto::setAreaTechnologySignDtoSet);
        secondFormLayout.add(areaTechnologySign);

        this.add(firstFormLayout, secondFormLayout);

        if (periodic) {
            Checkbox toBeInvoiced = new Checkbox(InvoiceState.TO_BE_INVOICED.getTranslatedName());
            binder.forField(toBeInvoiced).bind(InvoicingDto::isToBeInvoicedState, InvoicingDto::setToBeInvoicedState);
            this.add(toBeInvoiced);

            toBeInvoiced.addValueChangeListener(e ->
                    priceNoVat.setRequiredIndicatorVisible(Boolean.TRUE.equals(e.getValue())));
        }

        if (subjectDto != null) {
            subject.setValue(subjectDto);
            contract.setItems(contractDtoList.stream().filter(contractDto1 ->
                    contractDto1.getSubjectDto().getId().equals(subjectDto.getId())).toList().stream().toList());

            if (contractDto != null) {
                contract.setValue(contractDto);

                startDate.setValue(contractDto.getEffectStart());
                endDate.setValue(contractDto.getEndContract());
                maturityInvoice.setValue(contractDto.getMaturityInvoice());
                areaTechnologySign.setAreaTechnologySignDtoList(areaTechnologyComponentOperation.getAreaTechnologySignDtoList(ObjectType.CONTRACT,
                        contractDto.getId()));
                areaTechnologySign.initComponent();
            }
        } else {
            contract.setItems(contractDtoList);
        }
    }

}
