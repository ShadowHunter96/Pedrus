package cz.bbn.cerberus.document.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.component.upload.Upload;
import com.vaadin.flow.component.upload.receivers.MemoryBuffer;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.GridReloadOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.document.DocumentComponentOperation;
import cz.bbn.cerberus.document.DocumentObjectEnum;
import cz.bbn.cerberus.document.dto.DocumentDto;
import cz.bbn.cerberus.document.dto.DocumentFileDto;
import cz.bbn.cerberus.document.interfaces.DialogSaveAction;
import cz.bbn.cerberus.document.interfaces.LinkActionByName;
import cz.bbn.cerberus.documenttype.dto.DocumentTypeDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.invoice.dto.InvoicePdfParser;
import cz.bbn.cerberus.invoice.ui.component.InvoiceFromPdfDialog;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.SerializationUtils;
import org.apache.pdfbox.cos.COSDocument;
import org.apache.pdfbox.io.RandomAccessFile;
import org.apache.pdfbox.pdfparser.PDFParser;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.text.PDFTextStripper;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

@Slf4j
public class DocumentUploadDialog extends AppDialog {

    private static final String INVOICE_DOCUMENT_TYPE_ID = "INVOICE";

    private final DocumentObjectEnum documentObjectEnum;
    private final String entityId;
    private final DocumentDto dto;
    private final DocumentDto originalDto;
    private final DocumentComponentOperation documentComponentOperation;
    private final AppEnv appEnv;
    private final DialogSaveAction<DocumentDto> saveAction;
    private final List<DocumentTypeDto> documentTypeDtoList;
    private final Button linkButton;
    private final LinkActionByName linkActionByName;
    private final String objectName;
    private final GridReloadOperation gridReloadOperation;
    private final ListService listService;

    public DocumentUploadDialog(DocumentObjectEnum documentObjectEnum, String entityId,
                                DocumentComponentOperation documentComponentOperation,
                                DocumentDto dto, AppEnv appEnv, DialogSaveAction<DocumentDto> saveAction,
                                List<DocumentTypeDto> documentTypeDtoList, Button linkButton,
                                LinkActionByName linkActionByName,
                                GridReloadOperation gridReloadOperation, ListService listService) {
        this.documentObjectEnum = documentObjectEnum;
        this.entityId = entityId;
        this.dto = dto;
        this.originalDto = SerializationUtils.clone(dto);
        this.documentComponentOperation = documentComponentOperation;
        this.appEnv = appEnv;
        this.saveAction = saveAction;
        this.documentTypeDtoList = documentTypeDtoList;
        this.linkButton = linkButton;
        this.linkActionByName = linkActionByName;
        this.listService = listService;
        this.objectName = documentComponentOperation.getInvoiceComponentOperation().getObjectName(documentObjectEnum);
        this.gridReloadOperation = gridReloadOperation;
        initComponent();
    }

    private void initComponent() {
        Binder<DocumentDto> binder = new Binder<>();

        setTitle(dto.isNew() ? Transl.get("Upload new document") : Transl.get("Edit uploaded document"));
        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.setPadding(false);
        verticalLayout.setMargin(false);

        TextField name = new TextField(Transl.get("Name"));
        name.addValueChangeListener(textFieldStringComponentValueChangeEvent -> linkButton.setVisible(false));
        name.setMaxLength(100);
        name.setWidthFull();
        binder.forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(DocumentDto::getName, DocumentDto::setName);
        verticalLayout.add(name);

        ComboBox<DocumentTypeDto> documentTypes = new ComboBox<>(Transl.get("Document type"));
        documentTypes.setItems(documentTypeDtoList);
        documentTypes.setItemLabelGenerator(DocumentTypeDto::getName);
        binder.forField(documentTypes).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY)).
                bind(DocumentDto::getDocumentTypeDto, DocumentDto::setDocumentTypeDto);
        verticalLayout.add(documentTypes);

        Checkbox createInvoice = new Checkbox(Transl.get("Create invoice"));
        createInvoice.setVisible(false);
        createInvoice.setValue(false);

        documentTypes.addValueChangeListener(e ->
                createInvoice.setVisible(e.getValue() != null &&
                        INVOICE_DOCUMENT_TYPE_ID.equals(e.getValue().getId())
                        && DomainEnum.CONTRACT_DOMAIN_NAME.getValue().equals(objectName)));

        MemoryBuffer memoryBuffer = new MemoryBuffer();
        Upload upload = new Upload(memoryBuffer);
        upload.setDropAllowed(true);
        int maxFileSizeInBytes = 20 * 1024 * 1024; // 20MB
        upload.setMaxFileSize(maxFileSizeInBytes);

        upload.addFileRejectedListener(event -> {
            String errorMessage = event.getErrorMessage();
            ErrorNotification.show(Transl.get(errorMessage), appEnv);
        });

        documentTypes.addValueChangeListener(e -> createInvoice.setVisible(e.getValue() != null &&
                INVOICE_DOCUMENT_TYPE_ID.equals(e.getValue().getId())));

        AtomicReference<String> fileType = new AtomicReference<>();
        AtomicReference<Long> contentLength = new AtomicReference<>();

        upload.addSucceededListener(event -> {
            name.setValue(event.getFileName());
            contentLength.set(event.getContentLength());

            String fileName = event.getFileName();
            String fileTypeActual = fileName.substring(fileName.lastIndexOf(".") + 1);
            fileType.set(fileTypeActual);
            linkButton.setVisible(false);
        });

        verticalLayout.add(createInvoice, upload);

        setContent(verticalLayout);

        Button save = VaadinComponents.getSubmitButton();
        save.setDisableOnClick(true);
        save.addClickListener(buttonClickEvent -> {
            if (binder.validate().isOk()) {
                dto.setSize(contentLength.get());
                dto.setFileType(fileType.get());

                List<Long> allowedFormatIdList = documentTypes.getValue().getAllowedFormatsList();
                List<EnumerationDto> allowedFormatList = listService.getEnumerationDtoList("DOCUMENT_FORMAT");
                boolean allowedFormat = checkAllowedFormats(allowedFormatIdList, allowedFormatList);
                if (!allowedFormat) {
                    ErrorNotification.show(Transl.get(
                            "The {0} file format is not allowed for this document type", dto.getFileType()), appEnv);
                    save.setEnabled(true);
                    return;
                }

                DocumentFileDto tempDocumentFileDto = new DocumentFileDto();
                tempDocumentFileDto.setName(dto.getName());
                tempDocumentFileDto.setFileData(memoryBuffer.getInputStream());

                dto.setDocumentFileDto(tempDocumentFileDto);
                boolean result = saveAction.save(dto, originalDto);
                if (result) {
                    this.close();
                    if (documentTypes.getValue() != null
                            && INVOICE_DOCUMENT_TYPE_ID.equals(documentTypes.getValue().getId())
                            && Boolean.TRUE.equals(createInvoice.getValue()) &&
                            DomainEnum.CONTRACT_DOMAIN_NAME.getValue().equals(documentComponentOperation
                                    .getInvoiceComponentOperation().getObjectName(documentObjectEnum))) {
                        createInvoice();
                    }
                }
            }
            save.setEnabled(true);
        });

        binder.setBean(dto);

        Button close = VaadinComponents.getCloseButton();
        close.addClickListener(buttonClickEvent -> this.close());

        linkButton.setVisible(false);
        linkButton.addClickListener(buttonClickEvent ->
                linkActionByName.linkAction(name.getValue(), originalDto.getName(), documentObjectEnum, entityId));
        addButtons(close, linkButton, save);
    }

    private boolean checkAllowedFormats(List<Long> allowedFormatIdList, List<EnumerationDto> allowedFormatList) {
        boolean allowedFormat = false;
        if (allowedFormatIdList != null) {
            for (Long id : allowedFormatIdList) {
                Optional<EnumerationDto> enumerationDto = allowedFormatList.stream()
                        .filter(actualEnumerationDto -> actualEnumerationDto.getId().equals(id)).findAny();
                if (enumerationDto.isPresent() && enumerationDto.get().getValue() != null) {
                    List<String> names = Arrays.stream(enumerationDto.get().getValue().split(",")).toList();
                    if (names.contains(dto.getFileType())) {
                        allowedFormat = true;
                        break;
                    }
                }
            }
        }
        return allowedFormat;
    }

    private void createInvoice() {
        try {

            InputStream inputStream = documentComponentOperation.getItemActionDocumentFile().getItem(dto.getName())
                    .getDocumentFileDto().getFileData();

            File file = File.createTempFile("temp", "pdf");
            FileUtils.copyInputStreamToFile(inputStream, file);
            PDFParser parser = new PDFParser(new RandomAccessFile(file, "r"));
            parser.parse();
            COSDocument cosDoc = parser.getDocument();
            PDFTextStripper pdfTextStripper = new PDFTextStripper();
            PDDocument pdfText = new PDDocument(cosDoc);
            String parsedText = pdfTextStripper.getText(pdfText);

            InvoicePdfParser invoicePdfParser = new InvoicePdfParser(parsedText);
            ContractDto contractDto = listService.getContractList().stream().filter(
                    contractTypeDto -> contractTypeDto.getId().equals(entityId)).findAny().get();
            new InvoiceFromPdfDialog(invoicePdfParser, documentComponentOperation, dto, gridReloadOperation,
                    appEnv, contractDto);
        } catch (IOException e) {
            log.error("Invalid PDF file - invoice creation canceled", e);
            ErrorNotification.show(Transl.get("Invalid PDF file - invoice creation canceled"), appEnv);
        }
    }
}
