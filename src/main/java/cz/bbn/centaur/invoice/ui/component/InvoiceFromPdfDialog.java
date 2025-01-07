package cz.bbn.cerberus.invoice.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.NumberField;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.GridReloadOperation;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.validator.DoubleValueMinValidator;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.document.DocumentComponentOperation;
import cz.bbn.cerberus.document.dto.DocumentDto;
import cz.bbn.cerberus.document.ui.component.DocumentFileTypeEnum;
import cz.bbn.cerberus.document.ui.component.DocumentViewer;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.invoice.dto.InvoicePdfParser;
import cz.bbn.cerberus.translation.Transl;

import java.time.LocalDate;

public class InvoiceFromPdfDialog extends AppDialog {

    private final DocumentComponentOperation documentComponentOperation;
    private final GridReloadOperation gridReloadOperation;
    private final AppEnv appEnv;

    private final InvoicePdfParser invoicePdfParser;
    private final ContractDto contractDto;
    private final InvoiceDto invoiceDto = new InvoiceDto();
    private final Binder<InvoiceDto> binder = new Binder<>();
    private final DocumentDto documentDto;

    private final Checkbox payed = new Checkbox(Transl.get("Payed"));
    private final Checkbox transferProtocol = new Checkbox(Transl.get("Transfer protocol"));

    public InvoiceFromPdfDialog(InvoicePdfParser invoicePdfParser,
                                DocumentComponentOperation documentComponentOperation,
                                DocumentDto documentDto, GridReloadOperation gridReloadOperation,
                                AppEnv appEnv, ContractDto contractDto) {
        this.invoicePdfParser = invoicePdfParser;
        this.documentComponentOperation = documentComponentOperation;
        this.documentDto = documentDto;
        this.gridReloadOperation = gridReloadOperation;
        this.appEnv = appEnv;
        this.contractDto = contractDto;
        initComponent();
    }

    private void initComponent() {

        setTitle(Transl.get("Create invoice from PDF"));

        invoiceDto.setContractDto(contractDto);
        invoiceDto.setDeleted(false);
        binder.setBean(invoiceDto);

        NumberField amount = new NumberField(Transl.get("Amount in CZK"));
        amount.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        ComboBox<Double> amountValues = new ComboBox<>(Transl.get("Amount parsed values"));
        amountValues.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());

        if (invoicePdfParser.getAmountList().isEmpty()) {
            amountValues.setReadOnly(true);
        } else {
            amountValues.setItems(invoicePdfParser.getAmountList());
            invoiceDto.setPriceTotal(invoicePdfParser.getAmountList().get(0));
            amountValues.setValue(invoicePdfParser.getAmountList().get(0));
        }
        binder.forField(amount).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .withValidator(new DoubleValueMinValidator(0))
                .bind(InvoiceDto::getPriceTotal, InvoiceDto::setPriceTotal);

        amountValues.addValueChangeListener(e -> amount.setValue(e.getValue()));

        DatePicker dueDatePicker;

        ComboBox<LocalDate> dueDateValues = new ComboBox<>(Transl.get("Due date parsed values"));

        if (invoicePdfParser.getDueDateList().isEmpty()) {
            dueDatePicker = VaadinComponents.getDatePicker(LocalDate.now());
            invoiceDto.setIssueDate(LocalDate.now());
            dueDateValues.setReadOnly(true);
        } else {
            dueDatePicker = VaadinComponents.getDatePicker(invoicePdfParser.getDueDateList().get(0));
            dueDateValues.setItems(invoicePdfParser.getDueDateList());
            dueDateValues.setItemLabelGenerator(AppUtils::formatDate);
            invoiceDto.setIssueDate(invoicePdfParser.getDueDateList().get(0));
            dueDateValues.setValue(invoicePdfParser.getDueDateList().get(0));
        }

        dueDatePicker.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());

        dueDateValues.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());

        dueDatePicker.setLabel(Transl.get("Due date"));

        binder.forField(dueDatePicker).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(InvoiceDto::getIssueDate, InvoiceDto::setIssueDate);

        dueDateValues.addValueChangeListener(e -> dueDatePicker.setValue(e.getValue()));

        TextArea description = new TextArea(Transl.get("Description"));
        description.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        description.setWidth(CssVariables.MEDIUM_FIELD_WIDTH.getValue());
        description.setMaxHeight("25em");
        description.setMinHeight("9em");
        binder.forField(description).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(InvoiceDto::getDescription, InvoiceDto::setDescription);

        TextField contract = new TextField();
        contract.setValue(contractDto.getName());
        contract.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        contract.setReadOnly(true);

        TextField document = new TextField();
        document.setValue(documentDto.getName());
        document.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());
        document.setReadOnly(true);

        payed.setValue(false);
        transferProtocol.setValue(false);

        HorizontalLayout horizontalLayout = new HorizontalLayout();
        horizontalLayout.setMargin(false);
        horizontalLayout.setPadding(false);
        VerticalLayout leftLayout = new VerticalLayout();
        leftLayout.setMargin(false);
        leftLayout.setPadding(false);
        leftLayout.add(amount, dueDatePicker, contract);
        VerticalLayout rightLayout = new VerticalLayout();
        rightLayout.setMargin(false);
        rightLayout.setPadding(false);
        rightLayout.add(amountValues, dueDateValues, document);
        horizontalLayout.add(leftLayout, rightLayout);

        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.setSizeFull();
        verticalLayout.setMargin(false);
        verticalLayout.setPadding(false);
        verticalLayout.add(horizontalLayout, description, payed, transferProtocol);

        VerticalLayout pdfLayout = new DocumentViewer(documentDto.getName(),
                documentComponentOperation, DocumentFileTypeEnum.PDF);

        HorizontalLayout dialogContent = new HorizontalLayout();
        dialogContent.setMargin(false);
        dialogContent.setPadding(false);
        dialogContent.add(pdfLayout, verticalLayout);
        dialogContent.setSizeFull();
        setContent(dialogContent);

        initButtonLayout();
        this.open();
    }

    private void initButtonLayout() {
        Button saveButton = VaadinComponents.getSubmitButton();
        saveButton.setDisableOnClick(true);
        saveButton.addClickListener(e -> {
            if (binder.validate().isOk()) {
                InvoiceDto dto = binder.getBean();
                dto.setTransferProtocol(transferProtocol.getValue());
                dto.setContractDto(contractDto);
                dto.setDocumentName(documentDto.getName());
                if (TextValues.EMPTY_VALUE.equals(dto.getDocumentName())) {
                    dto.setDocumentName(null);
                }
                SaveAction<InvoiceDto> saveAction = documentComponentOperation.getInvoiceComponentOperation()
                        .getSingleItemSaveAction();
                saveAction.saveItem(invoiceDto, new InvoiceDto());
                gridReloadOperation.reloadGrid();
                this.close();
                SuccessNotification.showSavingSuccess(appEnv);
            }
            saveButton.setEnabled(true);
        });

        addCloseButton();
        addButtons(saveButton);
    }
}
