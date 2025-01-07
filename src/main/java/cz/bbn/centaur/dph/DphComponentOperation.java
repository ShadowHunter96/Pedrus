package cz.bbn.cerberus.dph;

import com.vaadin.flow.component.UI;
import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.DphTabComponent;
import cz.bbn.cerberus.dph.ui.DphDetailView;
import cz.bbn.cerberus.invoice.InvoicingService;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.dph.dto.DphDto;
import cz.bbn.cerberus.dph.dto.DphFilterDto;
import cz.bbn.cerberus.dph.ui.component.DphFilterComponent;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
@Slf4j
public class DphComponentOperation {

    private final DphService dphService;
    private final InvoicingService invoicingService;
    private final AppEnv appEnv;

    public DphComponentOperation(DphService dphService, InvoicingService invoicingService, AppEnv appEnv) {
        this.dphService = dphService;
        this.invoicingService = invoicingService;
        this.appEnv = appEnv;
    }

    public SaveAction<DphDto> getSaveAction(AppDialog appDialog) {
        return (newDto, originalDto) -> {
            try {
                if (originalDto.getId() != null) {
                    dphService.updateDph(newDto, originalDto);
                    UI.getCurrent().navigate(
                            AdministrationView.ROUTE.concat("/").concat(String.valueOf(DphTabComponent.TAB_INDEX)));
                } else {
                    newDto.setAllowed(Boolean.TRUE);
                    dphService.saveDph(newDto);
                    if (appDialog != null) {
                        appDialog.showWarning(false);
                        appDialog.close();
                        UI.getCurrent().navigate(DphDetailView.ROUTE .concat("/").concat(newDto.getId()));
                    }
                }
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public ItemsAction<DphDto> getItemsAction(DphFilterComponent dphFilterComponent) {
        return (query, orderList) -> {
            DphFilterDto filter = dphFilterComponent.getDphFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return dphService.findDphDtoPage(filter);
        };
    }

    public ListAction<String> getInvoiceListAction() {
        return id -> {
            List<String> invoiceList = new ArrayList<>();
            List<InvoiceDto> invoiceDtoList = invoicingService.getInvoiceByVatId(id);
            for (InvoiceDto invoiceDto : invoiceDtoList) {
                invoiceList.add((invoiceDto.getInvoiceNo() != null && !"".equals(invoiceDto.getInvoiceNo()) ?
                        invoiceDto.getInvoiceNo().concat(" (").concat(String.valueOf(invoiceDto.getId())).concat(")") :
                        "".concat(String.valueOf(invoiceDto.getId()))));
            }
            return invoiceList;
        };
    }
}
