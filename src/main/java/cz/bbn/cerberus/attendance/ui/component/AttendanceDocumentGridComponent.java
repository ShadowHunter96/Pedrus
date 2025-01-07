package cz.bbn.cerberus.attendance.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.grid.GridSortOrder;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.provider.SortDirection;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.server.StreamResource;
import cz.bbn.cerberus.attendance.AttendanceComponentOperation;
import cz.bbn.cerberus.attendance.dto.AttendanceSimpleDocumentDto;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.DeleteConfirmDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.document.ui.component.DocumentPreviewDialog;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import org.vaadin.olli.FileDownloadWrapper;

import java.util.ArrayList;
import java.util.List;

public class AttendanceDocumentGridComponent extends AppInfiniteGrid<AttendanceSimpleDocumentDto> {

    private final AttendanceComponentOperation attendanceComponentOperation;

    public AttendanceDocumentGridComponent(DeleteAction deleteAction, AppEnv appEnv,
                                           ItemsAction<AttendanceSimpleDocumentDto> itemsAction,
                                           AttendanceComponentOperation attendanceComponentOperation) {
        super(deleteAction, appEnv, itemsAction);
        this.attendanceComponentOperation = attendanceComponentOperation;
        initGrid();
    }

    private void initGrid() {

        addColumn(AttendanceSimpleDocumentDto::getName)
                .setSortable(true).setHeader(Transl.get("Name")).setKey("name");
        Column<AttendanceSimpleDocumentDto> date = addColumn(attendanceDocumentDto -> AppUtils.formatDateTime(attendanceDocumentDto.getDate(), true))
                .setSortable(true).setHeader(Transl.get("Date")).setKey("date");
        addColumn(attendanceDocumentDto -> attendanceDocumentDto.getUserDto().getName())
                .setSortable(true).setHeader(Transl.get("Creator")).setKey("userEntity.name");
        addColumn(new ComponentRenderer<>(attendanceDocumentDto ->
                VaadinComponents.getCheckUncheckLayoutNullTrue(attendanceDocumentDto.getDeleted())))
                .setHeader(Transl.get("Deleted"))
                .setWidth(VaadinValues.COLUMN_ICON_SIZE_MEDIUM)
                .setFlexGrow(0)
                .setSortable(true).setKey("deleted");
        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_SMALL)
                .setFlexGrow(1).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(event -> gridClicked(event.getItem()));
        GridSortOrder<AttendanceSimpleDocumentDto> sortOrder = new GridSortOrder<>(date, SortDirection.DESCENDING);
        List<GridSortOrder<AttendanceSimpleDocumentDto>> sortList = new ArrayList<>();
        sortList.add(sortOrder);
        sort(sortList);
    }

    private HorizontalLayout getGridButtons(AttendanceSimpleDocumentDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");

        Button showFileButton = VaadinComponents.getButton(VaadinIcon.EYE.create());
        showFileButton.getElement().setProperty(TextValues.TITLE, Transl.get("Show"));
        showFileButton.addClickListener(e -> new DocumentPreviewDialog(clickedItem.getName(),
                attendanceComponentOperation.getPdf(clickedItem.getId())));
        buttons.add(showFileButton);

        Button downloadButton = VaadinComponents.getButton(VaadinIcon.DOWNLOAD.create());
        downloadButton.getElement().setProperty(TextValues.TITLE, Transl.get("Download"));
        StreamResource excelResource = new StreamResource(
                clickedItem.getName().concat(".pdf"),
                () -> attendanceComponentOperation.getPdf(clickedItem.getId()));

        FileDownloadWrapper buttonPdfWrapper = new FileDownloadWrapper(excelResource);
        buttonPdfWrapper.wrapComponent(downloadButton);
        buttons.add(buttonPdfWrapper);

        if (SecurityUtils.hasPermission(Permission.ATTENDANCE_DOCUMENT_DELETE)) {
            Button delete = VaadinComponents.getDeleteButton();
            AppUtils.addRfClassToGridButton(delete, String.valueOf(clickedItem.getId()));
            delete.addClickListener(buttonClickEvent -> {
                DeleteConfirmDialog deleteConfirmDialog =
                        new DeleteConfirmDialog(this, String.valueOf(clickedItem.getId()), Transl.get(
                                "Are you sure you want to delete report document {0} ",
                                clickedItem.getName()), getAppEnv(), true);
                deleteConfirmDialog.open();
            });
            delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete report document"));
            buttons.add(delete);
        }
        return buttons;
    }

    private void gridClicked(AttendanceSimpleDocumentDto clickedItem) {
        new DocumentPreviewDialog(clickedItem.getName(),
                attendanceComponentOperation.getPdf(clickedItem.getId()));
    }
}
