package cz.bbn.cerberus.invoice.ui.component;

import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.ConfirmAction;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;

public interface InvoicingDetailListener {

    ConfirmAction getInvoicedAction(Binder<InvoiceDto> binder, AppDialog dialog);

    ConfirmAction getPayedAction(Binder<InvoiceDto> binder, AppDialog dialog);
}
