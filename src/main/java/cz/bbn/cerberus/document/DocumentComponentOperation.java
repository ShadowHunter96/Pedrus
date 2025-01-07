package cz.bbn.cerberus.document;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.interfaces.GetItemAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.GridReloadOperation;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.OpenDialogAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.notification.WarningNotification;
import cz.bbn.cerberus.contactperson.ContactPersonService;
import cz.bbn.cerberus.contract.ContractService;
import cz.bbn.cerberus.document.dto.DocumentByObjectDto;
import cz.bbn.cerberus.document.dto.DocumentDto;
import cz.bbn.cerberus.document.dto.DocumentFilterDto;
import cz.bbn.cerberus.document.interfaces.DeleteOrRestoreAction;
import cz.bbn.cerberus.document.interfaces.DeleteOrUnlinkAction;
import cz.bbn.cerberus.document.interfaces.DialogSaveAction;
import cz.bbn.cerberus.document.interfaces.LinkAction;
import cz.bbn.cerberus.document.interfaces.LinkActionByName;
import cz.bbn.cerberus.document.persistance.entity.DocumentByObjectId;
import cz.bbn.cerberus.document.ui.component.DocumentFilterComponent;
import cz.bbn.cerberus.document.ui.component.DocumentGridComponent;
import cz.bbn.cerberus.document.ui.component.DocumentUploadDialog;
import cz.bbn.cerberus.documenttype.DocumentTypeService;
import cz.bbn.cerberus.documenttype.dto.DocumentTypeDto;
import cz.bbn.cerberus.invoice.InvoiceComponentOperation;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.project.ProjectService;
import cz.bbn.cerberus.subject.SubjectService;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.tmatesoft.svn.core.SVNException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

@Component
@Slf4j
public class DocumentComponentOperation {

    private final DocumentService documentService;
    private final DocumentTypeService documentTypeService;
    private final ProjectService projectService;
    private final ContactPersonService contactPersonService;
    private final SubjectService subjectService;
    private final AppEnv appEnv;
    private final ContractService contractService;
    private final InvoiceComponentOperation invoiceComponentOperation;
    private final ListService listService;

    public DocumentComponentOperation(DocumentService documentService,
                                      DocumentTypeService documentTypeService,
                                      ProjectService projectService,
                                      ContactPersonService contactPersonService,
                                      SubjectService subjectService, AppEnv appEnv,
                                      ContractService contractService,
                                      InvoiceComponentOperation invoiceComponentOperation, ListService listService) {
        this.documentService = documentService;
        this.documentTypeService = documentTypeService;
        this.projectService = projectService;
        this.contactPersonService = contactPersonService;
        this.subjectService = subjectService;
        this.appEnv = appEnv;
        this.contractService = contractService;
        this.invoiceComponentOperation = invoiceComponentOperation;
        this.listService = listService;
    }

    public List<DocumentTypeDto> getDocumentTypeDtoList() {
        return documentTypeService.findAll();
    }

    public ListAction<ItemDto> getListAction() {
        return id -> {
            switch (DocumentObjectEnum.valueOf(id)) {
                case PROJECT:
                    return projectService.findItemDtoList();
                case CONTACT_PERSON:
                    return contactPersonService.findItemDtoList();
                case SUBJECT:
                    return subjectService.findItemDtoList();
                case CONTRACT:
                    return contractService.findItemDtoList();

                default:
                    return new ArrayList<>();
            }
        };
    }

    public ItemsAction<DocumentDto> getItemsAction(DocumentFilterComponent filterComponent) {
        return (query, orderList) -> {
            DocumentFilterDto filter = filterComponent.getDocumentFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return documentService.findDocumentDtoPage(filter);
        };
    }

    public ItemsAction<DocumentDto> getItemsAction(String id, DocumentObjectEnum documentObjectEnum) {
        return (query, orderList) -> {
            DocumentFilterDto filter = new DocumentFilterDto();
            filter.setObjectId(id);
            filter.setObjectType(documentObjectEnum);
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return documentService.findDocumentDtoPage(filter);
        };
    }

    public DialogSaveAction<DocumentDto> getSaveAction(String id, DocumentGridComponent grid,
                                                       Button linkButton, DocumentObjectEnum documentObjectEnum) {
        return (dto, originalDto) -> {
            dto.setEntityId(id);
            dto.setDocumentObjectEnum(documentObjectEnum);
            dto.setDeleted(Boolean.FALSE);
            try {
                if (!dto.isNew()) {
                    documentService.updateDocument(dto, originalDto);
                } else {
                    List<DocumentByObjectDto> documentByObjectDtoList = new ArrayList<>();
                    DocumentByObjectDto documentByObjectDto = new DocumentByObjectDto(id,
                            documentObjectEnum);
                    documentByObjectDtoList.add(documentByObjectDto);
                    dto.setDocumentByObjectDtoList(documentByObjectDtoList);

                    documentService.saveDocument(dto);
                }
                SuccessNotification.showSavingSuccess(appEnv);
                grid.loadData();
                return true;
            } catch (SystemException e) {
                if (e.getErrorCode() == ErrorCode.DOCUMENT_NAME_SIZE_ALREADY_EXISTS) {
                    WarningNotification.show(Transl.get(e.getMessage()), appEnv);
                    linkButton.setVisible(true);
                } else {
                    WarningNotification.show(Transl.get(e.getMessage()), appEnv);
                }
                log.error(TextValues.SYSTEM_EXCEPTION, e);

            } catch (IOException | SVNException e) {
                log.error(TextValues.SYSTEM_EXCEPTION, e);
                ErrorNotification.show(e, appEnv);
            }
            return false;
        };
    }

    public DeleteOrUnlinkAction getDeleteOrUnlinkAction(DocumentObjectEnum documentObjectEnum) {
        return (id, entityId, unlink) -> {
            try {
                documentService.deleteOrUnlinkDocument(id, entityId, documentObjectEnum, unlink);
            } catch (SystemException e) {
                log.error(TextValues.SYSTEM_EXCEPTION, e);
                ErrorNotification.show(e, appEnv);
            }
        };
    }

    public DeleteOrRestoreAction getDeleteOrRestoreAction() {
        return (name, restore) -> {
            try {
                documentService.deleteOrRestoreDocument(name, restore);
            } catch (SystemException | IOException | SVNException e) {
                log.error(TextValues.SYSTEM_EXCEPTION, e);
                ErrorNotification.show(e, appEnv);
            }
        };
    }

    public LinkAction getLinkAction() {
        return this::linkDocument;
    }

    public LinkActionByName getLinkActionByName() {
        return (name, oldDocumentId, documentEntityEnum, entityId) ->
                linkDocument(documentEntityEnum.name(), name, entityId);
    }

    public GetItemAction<DocumentDto> getItemActionDocumentFile() {
        return id -> {
            try {
                return documentService.getDocumentWithFileDto(id);
            } catch (SystemException | IOException | SVNException e) {
                log.error(TextValues.SYSTEM_EXCEPTION, e);
                ErrorNotification.show(e, appEnv);
            }
            return null;
        };
    }

    public OpenDialogAction getEditEvent(DocumentUploadDialog documentUploadDialog,
                                         Button linkButton,
                                         DocumentGridComponent grid,
                                         String entityId,
                                         DocumentObjectEnum documentObjectEnum,
                                         GridReloadOperation gridReloadOperation) {
        return id -> {
            final DocumentDto documentDto;
            try {
                documentDto = documentService.getDocumentDto(id);
                dialogAction(documentDto, documentUploadDialog, linkButton, grid, entityId, documentObjectEnum,
                        gridReloadOperation);
            } catch (SystemException e) {
                log.error(TextValues.SYSTEM_EXCEPTION, e);
                ErrorNotification.show(e, appEnv);
            }
        };
    }

    public InvoiceComponentOperation getInvoiceComponentOperation() {
        return this.invoiceComponentOperation;
    }

    private void dialogAction(DocumentDto documentDto,
                              DocumentUploadDialog documentUploadDialog,
                              Button linkButton,
                              DocumentGridComponent grid,
                              String id,
                              DocumentObjectEnum documentObjectEnum, GridReloadOperation gridReloadOperation) {
        documentUploadDialog = new DocumentUploadDialog(documentObjectEnum,
                id,
                this,
                documentDto,
                appEnv,
                getSaveAction(id, grid, linkButton, documentObjectEnum),
                getDocumentTypeDtoList(),
                linkButton,
                getLinkActionByName(),
                gridReloadOperation,
                listService);
        documentUploadDialog.open();
    }

    private void linkDocument(String type, String documentName, String entityId) {
        DocumentByObjectId documentByObjectId =
                new DocumentByObjectId(documentName, entityId, DocumentObjectEnum.valueOf(type));
        documentByObjectId.setDocumentName(documentName);
        documentService.linkDocument(documentByObjectId);
        SuccessNotification.show(Transl.get("Document was linked"), appEnv);
    }
}
