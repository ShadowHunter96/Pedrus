package cz.bbn.cerberus.document.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.server.StreamResource;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.interfaces.GetItemAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.OpenDialogAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.document.DocumentComponentOperation;
import cz.bbn.cerberus.document.dto.DocumentDto;
import cz.bbn.cerberus.document.interfaces.DeleteOrRestoreAction;
import cz.bbn.cerberus.document.interfaces.DeleteOrUnlinkAction;
import cz.bbn.cerberus.document.interfaces.LinkAction;
import cz.bbn.cerberus.documenttype.dto.DocumentTypeDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import org.vaadin.olli.FileDownloadWrapper;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

public class DocumentGridComponent extends AppInfiniteGrid<DocumentDto> {

    private OpenDialogAction openDialogAction;
    private final DeleteOrUnlinkAction deleteOrUnlinkAction;
    private final DocumentComponentOperation documentComponentOperation;
    private final String entityId;
    private final Permission uploadPermission;
    private final Permission downloadPermission;
    private final Permission deletePermission;
    private final ListAction<ItemDto> listAction;
    private final LinkAction linkAction;
    private final GetItemAction<DocumentDto> getItemAction;
    private final DeleteOrRestoreAction deleteOrRestoreAction;
    private final List<DocumentTypeDto> documentTypeDtoList;

    public DocumentGridComponent(AppEnv appEnv, DocumentComponentOperation documentComponentOperation,
                                 ItemsAction<DocumentDto> itemsAction,
                                 DeleteOrUnlinkAction deleteOrUnlinkAction, Permission downloadPermission,
                                 Permission uploadPermission, Permission deletePermission, String entityId,
                                 ListAction<ItemDto> listAction, LinkAction linkAction,
                                 GetItemAction<DocumentDto> getItemAction, List<DocumentTypeDto> documentTypeDtoList) {
        super(appEnv, itemsAction);
        this.deleteOrUnlinkAction = deleteOrUnlinkAction;
        this.uploadPermission = uploadPermission;
        this.downloadPermission = downloadPermission;
        this.deletePermission = deletePermission;
        this.documentComponentOperation = documentComponentOperation;
        this.entityId = entityId;
        this.listAction = listAction;
        this.linkAction = linkAction;
        this.getItemAction = getItemAction;
        this.documentTypeDtoList = documentTypeDtoList;
        this.deleteOrRestoreAction = null;
        initGrid();
    }

    public DocumentGridComponent(AppEnv appEnv, DocumentComponentOperation documentComponentOperation,
                                 ItemsAction<DocumentDto> itemsAction,
                                 ListAction<ItemDto> listAction, Permission uploadPermission,
                                 Permission downloadPermission, LinkAction linkAction,
                                 GetItemAction<DocumentDto> getItemAction, DeleteOrRestoreAction deleteOrRestoreAction,
                                 List<DocumentTypeDto> documentTypeDtoList) {
        super(appEnv, itemsAction);
        this.listAction = listAction;
        this.uploadPermission = uploadPermission;
        this.downloadPermission = downloadPermission;
        this.linkAction = linkAction;
        this.getItemAction = getItemAction;
        this.documentTypeDtoList = documentTypeDtoList;
        this.documentComponentOperation = documentComponentOperation;
        this.openDialogAction = null;
        this.deletePermission = null;
        this.deleteOrUnlinkAction = null;
        this.entityId = null;
        this.deleteOrRestoreAction = deleteOrRestoreAction;
        initGrid();
    }

    private void initGrid() {
        addColumn(DocumentDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        addColumn(DocumentDto::getFileType).setHeader(Transl.get("File type")).setSortable(true).setKey("fileType");

        addColumn(documentDto -> {
            if (documentDto.getDocumentType() != null) {
                AtomicReference<String> name = new AtomicReference<>("");
                documentTypeDtoList.forEach(documentTypeDto -> {
                    if (documentDto.getDocumentType().equals(documentTypeDto.getId())) {
                        name.set(documentTypeDto.getName());
                    }
                });
                return name;
            }
            return "";
        }).setHeader(Transl.get("Document type"))
                .setSortable(true)
                .setKey("documentTypeDtp");

        addColumn(documentDto -> new BigDecimal(
                documentDto.getSize())
                .divide(BigDecimal.valueOf(1024))
                .divide(BigDecimal.valueOf(1024))
                .round(new MathContext(2, RoundingMode.HALF_UP)))
                .setHeader(Transl.get("Size MB")).setSortable(true).setKey("size");

        setColumnReorderingAllowed(true);
        setMultiSort(true);

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEGA)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        if (entityId != null) {
            addItemDoubleClickListener(event -> openDialogAction.doAction(event.getItem().getName()));
        }

    }

    private HorizontalLayout getGridButtons(DocumentDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");

        Button showFileButton = VaadinComponents.getButton(VaadinIcon.EYE.create());
        showFileButton.getElement().setProperty(TextValues.TITLE, Transl.get("Show"));
        showFileButton.addClickListener(e -> new DocumentPreviewDialog(clickedItem.getName(),
                documentComponentOperation, DocumentFileTypeEnum.getTypeByExtension(clickedItem.getFileType())));
        buttons.add(showFileButton);

        if (DocumentFileTypeEnum.getTypeByExtension(clickedItem.getFileType()) == DocumentFileTypeEnum.UNSUPPORTED) {
            showFileButton.setClassName("button-hidden");
        }

        if (SecurityUtils.hasPermission(downloadPermission)) {
            Button downloadButton = VaadinComponents.getButton(VaadinIcon.DOWNLOAD.create());
            downloadButton.getElement().setProperty(TextValues.TITLE, Transl.get("Download"));
            StreamResource resource = new StreamResource(
                    clickedItem.getName(),
                    () -> getItemAction.getItem(clickedItem.getName()).getDocumentFileDto().getFileData());

            FileDownloadWrapper buttonWrapper = new FileDownloadWrapper(resource);
            buttonWrapper.wrapComponent(downloadButton);
            buttons.add(buttonWrapper);
        }

        if (openDialogAction != null) {

            if (SecurityUtils.hasPermission(deletePermission)) {
                Button delete = VaadinComponents.getDeleteButton();
                AppUtils.addRfClassToGridButton(delete, clickedItem.getName());
                delete.addClickListener(buttonClickEvent -> {
                    DeleteOrUnlinkDialog dialog = new DeleteOrUnlinkDialog(deleteOrUnlinkAction,
                            clickedItem.getName(), entityId, this);
                    dialog.open();
                });
                delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete or unlink document"));
                buttons.add(delete);
            }
        }

        if (SecurityUtils.hasPermission(uploadPermission)) {
            Button link = VaadinComponents.getButton(VaadinIcon.LINK.create());
            AppUtils.addRfClassToGridButton(link, clickedItem.getName());
            link.addClickListener(buttonClickEvent -> {
                LinkDocumentDialog linkDocumentDialog =
                        new LinkDocumentDialog(listAction, linkAction, clickedItem.getName());
                linkDocumentDialog.open();
            });
            link.getElement().setProperty(TextValues.TITLE, Transl.get("Link document"));
            buttons.add(link);
        }

        if (SecurityUtils.hasPermission(uploadPermission) && clickedItem.getDeleted()) {
            Button deleteOrRestore = VaadinComponents.getButton(VaadinIcon.FILE_REFRESH.create());
            AppUtils.addRfClassToGridButton(deleteOrRestore, clickedItem.getName());
            deleteOrRestore.addClickListener(buttonClickEvent -> {
                DeleteOrRestoreDialog deleteOrRestoreDialog =
                        new DeleteOrRestoreDialog(this, deleteOrRestoreAction, clickedItem.getName());
                deleteOrRestoreDialog.open();
            });
            deleteOrRestore.getElement().setProperty(TextValues.TITLE, Transl.get("Delete or restore"));
            buttons.add(deleteOrRestore);
        }
        return buttons;
    }


    public void setOpenDialogAction(OpenDialogAction openDialogAction) {
        this.openDialogAction = openDialogAction;
    }
}
