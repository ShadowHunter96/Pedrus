package cz.bbn.cerberus.document.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.GridReloadOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.document.DocumentComponentOperation;
import cz.bbn.cerberus.document.DocumentObjectEnum;
import cz.bbn.cerberus.document.dto.DocumentDto;
import cz.bbn.cerberus.documenttype.dto.DocumentTypeDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

@Slf4j
public class DocumentComponent extends VerticalLayout {

    private final DocumentComponentOperation documentComponentOperation;
    private final String id;
    private final AppEnv appEnv;
    private final DocumentObjectEnum documentObjectEnum;
    private final Permission uploadPermission;
    private final Permission deletePermission;
    private final Permission downloadPermission;
    private final boolean customPermission;
    private final Button addDocument;
    private final ListService listService;
    private final GridReloadOperation gridReloadOperation;

    private DocumentUploadDialog documentUploadDialog;
    private DocumentGridComponent grid;
    private Button linkButton;

    public DocumentComponent(DocumentComponentOperation documentComponentOperation,
                             String id, AppEnv appEnv, DocumentObjectEnum documentObjectEnum,
                             Permission uploadPermission, Permission deletePermission, Permission downloadPermission,
                             boolean customPermission, Button addDocument, ListService listService) {
        this.documentComponentOperation = documentComponentOperation;
        this.id = id;
        this.appEnv = appEnv;
        this.documentObjectEnum = documentObjectEnum;
        this.uploadPermission = uploadPermission;
        this.deletePermission = deletePermission;
        this.downloadPermission = downloadPermission;
        this.addDocument = addDocument;
        this.listService = listService;
        this.gridReloadOperation = getEmptyAction();
        this.customPermission = customPermission;
        initComponent();
    }

    public DocumentComponent(DocumentComponentOperation documentComponentOperation,
                             String id, AppEnv appEnv, DocumentObjectEnum documentObjectEnum,
                             Permission uploadPermission, Permission deletePermission, Permission downloadPermission,
                             GridReloadOperation gridReloadOperation, boolean customPermission, Button addDocument,
                             ListService listService) {
        this.documentComponentOperation = documentComponentOperation;
        this.id = id;
        this.appEnv = appEnv;
        this.documentObjectEnum = documentObjectEnum;
        this.uploadPermission = uploadPermission;
        this.deletePermission = deletePermission;
        this.downloadPermission = downloadPermission;
        this.gridReloadOperation = gridReloadOperation;
        this.customPermission = customPermission;
        this.addDocument = addDocument;
        this.listService = listService;
        initComponent();
    }

    private void initComponent() {
        linkButton = VaadinComponents.getLinkButton(Transl.get("Link document"));
        grid = new DocumentGridComponent(appEnv, documentComponentOperation,
                documentComponentOperation.getItemsAction(id, documentObjectEnum),
                documentComponentOperation.getDeleteOrUnlinkAction(documentObjectEnum),
                downloadPermission,
                uploadPermission,
                deletePermission,
                id, documentComponentOperation.getListAction(),
                documentComponentOperation.getLinkAction(),
                documentComponentOperation.getItemActionDocumentFile(),
                documentComponentOperation.getDocumentTypeDtoList());
        grid.setOpenDialogAction(documentComponentOperation.getEditEvent(documentUploadDialog, linkButton, grid, id,
                documentObjectEnum, gridReloadOperation));

        List<DocumentTypeDto> documentTypeDtoList = documentComponentOperation.getDocumentTypeDtoList();
        if (SecurityUtils.hasPermission(uploadPermission) && customPermission) {
            addDocument.addClickListener(buttonClickEvent -> {
                DocumentDto documentDto = new DocumentDto();
                documentDto.setNew(true);
                documentUploadDialog =
                        new DocumentUploadDialog(documentObjectEnum, id, documentComponentOperation,
                                documentDto, appEnv,
                                documentComponentOperation.getSaveAction(id, grid, linkButton, documentObjectEnum),
                                documentTypeDtoList, linkButton,
                                documentComponentOperation.getLinkActionByName(), gridReloadOperation, listService);
                documentUploadDialog.open();
            });
            this.add(addDocument);
        }

        grid.setSizeFull();
        this.setSizeFull();
        setPadding(false);
        setMargin(false);
        this.add(grid);
    }

    private GridReloadOperation getEmptyAction() {
        return () -> {
        };
    }

    public DocumentGridComponent getGrid() {
        return grid;
    }
}
