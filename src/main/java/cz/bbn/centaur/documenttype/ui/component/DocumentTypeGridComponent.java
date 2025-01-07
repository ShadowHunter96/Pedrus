package cz.bbn.cerberus.documenttype.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.ChangeWarningDialog;
import cz.bbn.cerberus.commons.component.ui.dialog.DeleteConfirmDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.documenttype.dto.ChangeDocumentTypeDto;
import cz.bbn.cerberus.documenttype.dto.DocumentTypeDto;
import cz.bbn.cerberus.documenttype.ui.DocumentTypeDetailView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class DocumentTypeGridComponent extends AppInfiniteGrid<DocumentTypeDto> {

    private final ListAction<String> listAction;
    private final ListAction<DocumentTypeDto> listActionForChange;
    private final SaveAction<String> saveActionChange;

    public DocumentTypeGridComponent(DeleteAction deleteAction, AppEnv appEnv,
                                     ItemsAction<DocumentTypeDto> itemsAction,
                                     ListAction<String> listAction, ListAction<DocumentTypeDto> listActionForChange,
                                     SaveAction<String> saveActionChange) {
        super(deleteAction, appEnv, itemsAction);
        this.listAction = listAction;
        this.listActionForChange = listActionForChange;
        this.saveActionChange = saveActionChange;
        initGrid();
    }

    public void initGrid() {
        setSizeFull();
        addColumn(DocumentTypeDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(DocumentTypeDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        addColumn(DocumentTypeDto::getDescription).setHeader(Transl.get("Description")).setSortable(true)
                .setKey("description");
        addColumn(new ComponentRenderer<>(documentTypeDto -> VaadinComponents
                .getCheckUncheckLayoutNullTrue(documentTypeDto.getAllowed())))
                .setHeader(Transl.get("Allowed"))
                .setWidth(VaadinValues.COLUMN_ICON_SIZE_MEDIUM)
                .setFlexGrow(0)
                .setSortable(true).setKey("allowed");

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0)
                .setTextAlign(ColumnTextAlign.CENTER);

        setColumnReorderingAllowed(true);
        setMultiSort(true);

        addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));
    }

    private HorizontalLayout getGridButtons(DocumentTypeDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");

        if (SecurityUtils.hasPermission(Permission.CONTACT_PERSON_TYPE_DELETE)) {
            Button delete = VaadinComponents.getDeleteButton();
            AppUtils.addRfClassToGridButton(delete, clickedItem.getId());
            delete.addClickListener(buttonClickEvent -> {
                List<String> usedList = listAction.getList(clickedItem.getId());
                if (usedList.isEmpty()) {
                    DeleteConfirmDialog deleteConfirmDialog =
                            new DeleteConfirmDialog(this, clickedItem.getId(), Transl.get(
                                    "Are you sure you want to delete document type {0} ",
                                    clickedItem.getId()), getAppEnv(), true);
                    deleteConfirmDialog.open();
                } else {
                    ChangeWarningDialog warningDialog = getChangeWarningDialog(clickedItem, usedList);
                    warningDialog.open();
                }
            });
            delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete document type"));
            buttons.add(delete);
        }

        return buttons;
    }

    private ChangeWarningDialog getChangeWarningDialog(DocumentTypeDto clickedItem, List<String> usedList) {
        ComboBox<DocumentTypeDto> documentTypes = new ComboBox<>("Change type to");
        documentTypes.setItems(listActionForChange.getList(clickedItem.getId()));
        documentTypes.setItemLabelGenerator(DocumentTypeDto::getName);
        Binder<ChangeDocumentTypeDto> binder = new Binder<>();
        binder.forField(documentTypes).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY)).
                bind(ChangeDocumentTypeDto::getDocumentTypeDto, ChangeDocumentTypeDto::setDocumentTypeDto);
        binder.setBean(new ChangeDocumentTypeDto());

        Button changeButton = VaadinComponents.getButton("Change and delete");

        ChangeWarningDialog changeWarningDialog = new ChangeWarningDialog(usedList, documentTypes, changeButton,
                "Delete document type", "The document type is used", "Documents");

        changeButton.setIcon(VaadinIcon.EXCHANGE.create());
        changeButton.setDisableOnClick(true);
        changeButton.addClickListener(buttonClickEvent -> {
            if (binder.validate().isOk()) {
                saveActionChange.saveItem(documentTypes.getValue().getId(), clickedItem.getId());
                this.loadData();
                changeWarningDialog.close();
            }
            changeButton.setEnabled(true);
        });
        return changeWarningDialog;
    }

    private void gridClicked(String code) {
        UI.getCurrent().navigate(DocumentTypeDetailView.ROUTE + "/" + code);
    }
}
