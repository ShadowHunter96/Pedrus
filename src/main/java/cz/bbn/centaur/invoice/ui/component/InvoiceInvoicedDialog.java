package cz.bbn.cerberus.invoice.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.translation.Transl;

import java.time.LocalDate;

public class InvoiceInvoicedDialog extends AppDialog {

    private final InvoicingDetailListener listener;
    private final Binder<InvoiceDto> binder;
    private final InvoiceDto invoiceDto;

    public InvoiceInvoicedDialog(InvoicingDetailListener listener) {
        this.listener = listener;
        this.binder = new Binder<>();
        this.invoiceDto = new InvoiceDto();
        binder.setBean(invoiceDto);
        initComponent();
    }

    private void initComponent() {

        invoiceDto.setIssueDate(LocalDate.now());
        invoiceDto.setTaxDate(LocalDate.now());

        this.setMinWidth(VaadinValues.DIALOG_MIN_WIDTH);
        this.setWidth("30%");

        this.setTitle(Transl.get("Invoiced"));

        InvoiceInvoicedComponent invoicedComponent = new InvoiceInvoicedComponent(invoiceDto, binder, false);
        invoicedComponent.setWidth("calc(100% - 4px)");

        setContent(invoicedComponent);

        Button submitButton = VaadinComponents.getConfirmButton();
        submitButton.addClickListener(e -> listener.getInvoicedAction(binder, this).confirm());

        addButtons(submitButton);
        addCloseButton();

    }
}
