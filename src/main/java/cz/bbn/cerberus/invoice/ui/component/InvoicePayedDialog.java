package cz.bbn.cerberus.invoice.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.translation.Transl;

import java.time.LocalDate;

public class InvoicePayedDialog extends AppDialog {

    private final InvoicingDetailListener listener;

    public InvoicePayedDialog(InvoicingDetailListener listener) {
        this.listener = listener;
        initComponent();
    }

    private void initComponent() {
        Binder<InvoiceDto> binder = new Binder<>();
        InvoiceDto invoiceDto = new InvoiceDto();
        invoiceDto.setPaymentDate(LocalDate.now());
        binder.setBean(invoiceDto);

        setTitle(Transl.get("Payed"));

        this.setMinWidth(VaadinValues.DIALOG_MIN_WIDTH);
        this.setWidth("30%");

        DatePicker paymentDate = VaadinComponents.getDatePicker(LocalDate.now());
        paymentDate.setLabel(Transl.get("Payment date"));
        binder.forField(paymentDate).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(InvoiceDto::getPaymentDate, InvoiceDto::setPaymentDate);
        this.add(paymentDate);

        paymentDate.setWidth("calc(100% - 4px)");

        Button submitButton = VaadinComponents.getConfirmButton();
        submitButton.addClickListener(e -> listener.getPayedAction(binder, this).confirm());

        setContent(paymentDate);

        addButtons(submitButton);
        addCloseButton();
    }
}
