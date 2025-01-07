package cz.bbn.cerberus.contract.ui.component.tab;

import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.invoice.InvoiceComponentOperation;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.invoice.ui.component.InvoiceDtoGridComponent;

import java.util.Collections;

public class ContractInvoiceGridTab extends TabSimpleComponent {

    private final InvoiceComponentOperation invoiceComponentOperation;
    private final ContractDto contractDto;
    private final AppEnv appEnv;

    private InvoiceDtoGridComponent grid;

    public ContractInvoiceGridTab(InvoiceComponentOperation invoiceComponentOperation, ContractDto contractDto,
                                  AppEnv appEnv) {
        this.invoiceComponentOperation = invoiceComponentOperation;
        this.contractDto = contractDto;
        this.appEnv = appEnv;
        initTab();
    }

    private void initTab() {
        removeAll();
        setSizeFull();
        this.grid = new InvoiceDtoGridComponent(invoiceComponentOperation,
                Collections.singletonList(contractDto), appEnv);
        this.add(grid);
        loadData();
    }

    public void loadData() {
        this.grid.loadData();
    }

    public AppInfiniteGrid<InvoiceDto> getGrid() {
        return this.grid;
    }
}
