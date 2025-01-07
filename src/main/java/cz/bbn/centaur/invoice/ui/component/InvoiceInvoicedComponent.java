package cz.bbn.cerberus.invoice.ui.component;

import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.translation.Transl;

import java.time.LocalDate;
import java.util.Locale;

public class InvoiceInvoicedComponent extends FormLayout {

    private final InvoiceDto invoiceDto;
    private final Binder<InvoiceDto> binder;
    private final Integer daysToPay;
    private final boolean readOnly;

    private DatePicker issueDate;
    private DatePicker dueDateField;

    public InvoiceInvoicedComponent(InvoiceDto invoiceDto, Binder<InvoiceDto> binder, boolean readOnly) {
        this.invoiceDto = invoiceDto;
        this.binder = binder;
        this.daysToPay = invoiceDto.getDaysToPay();
        this.readOnly = readOnly;
        initComponent();
    }

    private void initComponent() {

        setWidthFull();
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);

        issueDate = VaadinComponents.getDatePicker(invoiceDto.getIssueDate());
        issueDate.setLabel(Transl.get("Issue date"));
        issueDate.setLocale(Locale.forLanguageTag(Transl.DEFAULT_LANG.toLowerCase()));

        DatePicker taxDate = VaadinComponents.getDatePicker(invoiceDto.getTaxDate());
        taxDate.setLabel(Transl.get("Tax date"));
        taxDate.setLocale(Locale.forLanguageTag(Transl.DEFAULT_LANG.toLowerCase()));

        LocalDate dueDate = null;
        if (daysToPay != null && issueDate.getValue() != null) {
            dueDate = issueDate.getValue().plusDays(daysToPay);
        }

        dueDateField = VaadinComponents.getDatePicker(dueDate);
        dueDateField.setLabel(Transl.get("Due date"));
        dueDateField.setLocale(Locale.forLanguageTag(Transl.DEFAULT_LANG.toLowerCase()));
        dueDateField.setReadOnly(true);

        DatePicker paymentDate = VaadinComponents.getDatePicker(invoiceDto.getPaymentDate());
        paymentDate.setLabel(Transl.get("Payment date"));
        paymentDate.setLocale(Locale.forLanguageTag(Transl.DEFAULT_LANG.toLowerCase()));

        TextField invoiceNo = new TextField(Transl.get("Invoice Number"));

        if (readOnly) {
            binder.forField(issueDate).bind(InvoiceDto::getIssueDate, InvoiceDto::setIssueDate);
            binder.forField(taxDate).bind(InvoiceDto::getTaxDate, InvoiceDto::setTaxDate);
            binder.forField(paymentDate).bind(InvoiceDto::getPaymentDate, InvoiceDto::setPaymentDate);
            binder.forField(invoiceNo).bind(InvoiceDto::getInvoiceNo, InvoiceDto::setInvoiceNo);
        } else {
            binder.forField(issueDate).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(InvoiceDto::getIssueDate, InvoiceDto::setIssueDate);
            binder.forField(taxDate).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(InvoiceDto::getTaxDate, InvoiceDto::setTaxDate);
            binder.forField(invoiceNo).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(InvoiceDto::getInvoiceNo, InvoiceDto::setInvoiceNo);
        }

        issueDate.setReadOnly(readOnly);
        taxDate.setReadOnly(readOnly);
        paymentDate.setReadOnly(readOnly);
        invoiceNo.setReadOnly(readOnly);

        if (readOnly) {
            add(issueDate, taxDate, dueDateField, paymentDate, invoiceNo);
        } else {
            add(issueDate, invoiceNo, taxDate);
        }

        issueDate.addValueChangeListener(e -> changeDueDate(daysToPay));
    }

    public void changeDueDate(Integer daysToPay) {
        LocalDate dueDate = null;
        if (daysToPay != null && issueDate.getValue() != null) {
            dueDate = issueDate.getValue().plusDays(daysToPay);
        }
        dueDateField.setValue(dueDate);
    }
}
