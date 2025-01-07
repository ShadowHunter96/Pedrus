package cz.bbn.cerberus.subject.ui.component.tab;

import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.invoice.InvoiceComponentOperation;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.invoice.ui.component.InvoiceDtoGridComponent;

import java.util.List;

public class SubjectInvoiceGridTab extends TabSimpleComponent {

    private InvoiceDtoGridComponent invoiceDtoGridComponent;

    private final InvoiceComponentOperation invoiceComponentOperation;
    private final AppEnv appEnv;
    private final List<ContractDto> contractDtoList;

    public SubjectInvoiceGridTab(InvoiceComponentOperation invoiceComponentOperation, AppEnv appEnv,
                                 List<ContractDto> contractDtoList) {
        this.invoiceComponentOperation = invoiceComponentOperation;
        this.appEnv = appEnv;
        this.contractDtoList = contractDtoList;
        initTab();
    }

    private void initTab() {
        removeAll();
        setSizeFull();
        this.invoiceDtoGridComponent = new InvoiceDtoGridComponent(invoiceComponentOperation, contractDtoList, appEnv);
        this.add(invoiceDtoGridComponent);
        this.setSizeFull();
    }

    @Override
    public void loadTab() {
        invoiceDtoGridComponent.loadData();
    }

    public AppInfiniteGrid<InvoiceDto> getGrid() {
        return invoiceDtoGridComponent;
    }
}
