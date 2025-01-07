package cz.bbn.cerberus.suppliertype.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.grid.ColumnTextAlign;
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
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.suppliertype.dto.ChangeSupplierTypeDto;
import cz.bbn.cerberus.suppliertype.dto.SupplierTypeDto;
import cz.bbn.cerberus.suppliertype.ui.SupplierTypeDetailView;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class SupplierTypeGridComponent extends AppInfiniteGrid<SupplierTypeDto> {

    private final ListAction<String> listAction;
    private final ListAction<SupplierTypeDto> listActionForChange;
    private final SaveAction<String> saveActionChange;

    public SupplierTypeGridComponent(DeleteAction deleteAction, AppEnv appEnv,
                                     ItemsAction<SupplierTypeDto> itemsAction,
                                     ListAction<String> listAction,
                                     ListAction<SupplierTypeDto> listActionForChange,
                                     SaveAction<String> saveActionChange) {
        super(deleteAction, appEnv, itemsAction);
        this.listAction = listAction;
        this.listActionForChange = listActionForChange;
        this.saveActionChange = saveActionChange;
        initGrid();
    }

    private void initGrid() {
        setSizeFull();
        addColumn(SupplierTypeDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(SupplierTypeDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        addColumn(new ComponentRenderer<>(supplierTypeDto -> VaadinComponents
                .getCheckUncheckLayoutNullTrue(supplierTypeDto.getAllowed())))
                .setHeader(Transl.get("Allowed"))
                .setWidth(VaadinValues.COLUMN_ICON_SIZE_MEDIUM)
                .setFlexGrow(0)
                .setSortable(true).setKey("allowed");

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        setColumnReorderingAllowed(true);
        setMultiSort(true);

        addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));
    }

    private HorizontalLayout getGridButtons(SupplierTypeDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");

        if (SecurityUtils.hasPermission(Permission.SUPPLIER_TYPE_DELETE)) {
            Button delete = VaadinComponents.getDeleteButton();
            AppUtils.addRfClassToGridButton(delete, clickedItem.getId());
            delete.addClickListener(buttonClickEvent -> {
                List<String> supplierList = listAction.getList(clickedItem.getId());
                if (supplierList.isEmpty()) {
                    DeleteConfirmDialog deleteConfirmDialog =
                            new DeleteConfirmDialog(this, clickedItem.getId(), Transl.get(
                                    "Are you sure you want to delete supplier type {0} ",
                                    clickedItem.getId()), getAppEnv(), true);
                    deleteConfirmDialog.open();
                } else {
                    ChangeWarningDialog changeWarningDialog = getChangeWarningDialog(clickedItem, supplierList);
                    changeWarningDialog.open();
                }
            });
            delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete supplier type"));
            buttons.add(delete);
        }

        return buttons;
    }

    private ChangeWarningDialog getChangeWarningDialog(SupplierTypeDto clickedItem, List<String> supplierIdList) {
        ComboBox<SupplierTypeDto> supplierTypeDtoComboBox = new ComboBox<>(Transl.get("Change type to"));
        supplierTypeDtoComboBox.setItems(listActionForChange.getList(clickedItem.getId()));
        supplierTypeDtoComboBox.setItemLabelGenerator(SupplierTypeDto::getName);
        Binder<ChangeSupplierTypeDto> binder = new Binder<>();
        binder.forField(supplierTypeDtoComboBox).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY)).
                bind(ChangeSupplierTypeDto::getSupplierTypeDto, ChangeSupplierTypeDto::setSupplierTypeDto);
        binder.setBean(new ChangeSupplierTypeDto());

        Button changeButton = VaadinComponents.getChangeButton(Transl.get("Change and delete"));

        ChangeWarningDialog changeWarningDialog = new ChangeWarningDialog(supplierIdList, supplierTypeDtoComboBox,
                changeButton, "Delete supplier type", "The supplier type is in use", "Suppliers");

        changeButton.setDisableOnClick(true);
        changeButton.addClickListener(buttonClickEvent -> {
            if (binder.validate().isOk()) {
                saveActionChange.saveItem(supplierTypeDtoComboBox.getValue().getId(), clickedItem.getId());
                this.loadData();
                changeWarningDialog.close();
            }
            changeButton.setEnabled(true);
        });
        return changeWarningDialog;
    }

    private void gridClicked(String code) {
        UI.getCurrent().navigate(SupplierTypeDetailView.ROUTE + "/" + code);
    }
}
