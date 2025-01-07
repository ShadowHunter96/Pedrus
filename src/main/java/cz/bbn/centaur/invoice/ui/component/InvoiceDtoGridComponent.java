package cz.bbn.cerberus.invoice.ui.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.DeleteConfirmDialog;
import cz.bbn.cerberus.commons.component.ui.enums.FilterBoolean;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.invoice.InvoiceComponentOperation;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.invoice.ui.InvoicingDetailView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

public class InvoiceDtoGridComponent extends AppInfiniteGrid<InvoiceDto> implements InvoicingListener {

    private final AppEnv appEnv;
    private final InvoiceComponentOperation invoiceComponentOperation;
    private final List<ContractDto> contractDtoList;

    public InvoiceDtoGridComponent(InvoiceComponentOperation invoiceComponentOperation,
                                   List<ContractDto> contractDtoList, AppEnv appEnv) {
        super(invoiceComponentOperation.getDeleteAction(), appEnv,
                invoiceComponentOperation.getItemsAction(contractDtoList));
        this.invoiceComponentOperation = invoiceComponentOperation;
        this.appEnv = appEnv;
        this.contractDtoList = contractDtoList;

        initGrid();
    }

    public InvoiceDtoGridComponent(InvoiceComponentOperation invoiceComponentOperation,
                                   InvoiceFilterComponent invoiceFilterComponent, List<ContractDto> contractDtoList,
                                   AppEnv appEnv) {
        super(invoiceComponentOperation.getDeleteAction(), appEnv,
                invoiceComponentOperation.getItemsAction(invoiceFilterComponent));
        this.invoiceComponentOperation = invoiceComponentOperation;
        this.contractDtoList = contractDtoList;
        this.appEnv = appEnv;

        initGrid();
    }

    private void initGrid() {
        addColumn(new ComponentRenderer<>(invoiceDto -> createColumn(AppUtils.formatDate(invoiceDto.getInvoicingDate()), invoiceDto)))
                .setHeader(Transl.get("Invoicing date")).setSortable(true).setKey("invoicingDate");
        addColumn(new ComponentRenderer<>(invoiceDto -> createColumn(AppUtils.formatDate(invoiceDto.getIssueDate()), invoiceDto)))
                .setHeader(Transl.get("Issue date")).setSortable(true).setKey("issueDate");
        addColumn(invoiceDto -> Transl.get(invoiceDto.getState().name())).setHeader(Transl.get("State")).setKey("state")
                .setSortable(true);
        if (contractDtoList != null) {
            addColumn(new ComponentRenderer<>(invoiceDto -> createColumn(invoiceDto.getContractDto().getName(), invoiceDto)))
                    .setHeader(Transl.get("Contract"))
                    .setSortable(true).setKey("contractId");
            addColumn(new ComponentRenderer<>(invoiceDto -> createColumn(invoiceDto.getContractDto().getSubjectDto().getName(), invoiceDto)))
                    .setHeader(Transl.get("Customer"));
            addColumn(new ComponentRenderer<>(invoiceDto -> createColumn(AppUtils.priceWithDecimal(invoiceDto.getPriceNoVat()), invoiceDto)))
                    .setHeader(Transl.get("Price without VAT"))
                    .setSortable(true).setKey("priceNoVat");
            addColumn(new ComponentRenderer<>(invoiceDto -> createColumn(invoiceDto.getAppCurrency() != null ? invoiceDto.getAppCurrency().name() : "", invoiceDto)))
                    .setHeader(Transl.get("Currency")).setSortable(true).setKey("appCurrency");
        }
        addColumn(new ComponentRenderer<>(this::getUserName)).setHeader(Transl.get("Owner")).setKey("owner.name")
                .setSortable(true);
        addColumn(invoiceDto -> FilterBoolean.getValueForGrid(invoiceDto.getTransferProtocol()))
                .setHeader(Transl.get("Transfer protocol")).setSortable(true).setKey("transferProtocol");
        addColumn(new ComponentRenderer<>(this::getGridButtons)).setHeader(Transl.get("Actions"))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(e ->
                UI.getCurrent().navigate(InvoicingDetailView.ROUTE + "/" + e.getItem().getId()));

    }

    private Component createColumn(String value, InvoiceDto dto){
        HorizontalLayout layout = new HorizontalLayout();
        layout.setPadding(false);
        layout.setMargin(false);
        layout.add(value);
        layout.setSizeFull();
        layout.getElement().setProperty(TextValues.TITLE, getTitle(dto));
        return layout;
    }

    private String getTitle(InvoiceDto dto){
        if(StringUtils.isEmpty(dto.getDescription()) && StringUtils.isEmpty(dto.getInvoiceNo())){
            return "";
        }
        return StringUtils.trimToEmpty(dto.getDescription())
                .concat(" - ").concat(StringUtils.trimToEmpty(dto.getInvoiceNo()));
    }

    private HorizontalLayout getGridButtons(InvoiceDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");
        buttons.setPadding(false);
        buttons.setMargin(false);

        if (SecurityUtils.hasPermission(Permission.CONTRACT_DELETE) && !Boolean.TRUE.equals(clickedItem.getDeleted())) {
            Button delete = VaadinComponents.getDeleteButton();
            AppUtils.addRfClassToGridButton(delete, String.valueOf(clickedItem.getId()));
            delete.addClickListener(buttonClickEvent -> {
                DeleteConfirmDialog deleteConfirmDialog =
                        new DeleteConfirmDialog(this, String.valueOf(clickedItem.getId()),
                                Transl.get("Are you sure you want to delete invoice ?"), appEnv, true);
                deleteConfirmDialog.open();
            });
            delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete invoice"));
            buttons.add(delete);
        }

        if (SecurityUtils.hasPermission(Permission.CONTRACT_UPLOAD_TO_POHODA)
                && !Boolean.TRUE.equals(clickedItem.getCreatedInPohoda())) {
            Button saveToPohoda = VaadinComponents.getButton(VaadinIcon.BOMB.create());
            AppUtils.addRfClassToGridButton(saveToPohoda, String.valueOf(clickedItem.getId()));
            saveToPohoda.getElement().setProperty(TextValues.TITLE, Transl.get("Save to pohoda"));
            saveToPohoda.setDisableOnClick(true);
            saveToPohoda.addClickListener(buttonClickEvent -> {
                invoiceComponentOperation.saveInvoiceToPohoda().saveItem(clickedItem, null);
                this.loadData();
                saveToPohoda.setEnabled(true);
            });
            buttons.add(saveToPohoda);
        }
        return buttons;
    }

    private Span getUserName(InvoiceDto dto) {
        Span span = new Span();
        if (dto.getUserDto() != null) {
            if (dto.getUserDto().getAcronym() != null && !dto.getUserDto().getAcronym().isEmpty()) {
                span.add(dto.getUserDto().getAcronym());
            } else {
                span.add(dto.getUserDto().getName());
            }
        }
        span.getElement().setProperty(TextValues.TITLE, getTitle(dto));
        return span;
    }

    @Override
    public void reloadData() {
        loadData();
    }
}
