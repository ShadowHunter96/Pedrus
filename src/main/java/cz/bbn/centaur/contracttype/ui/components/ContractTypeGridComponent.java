package cz.bbn.cerberus.contracttype.ui.components;

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
import cz.bbn.cerberus.contracttype.dto.ChangeContractTypeDto;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.contracttype.ui.ContractTypeDetailView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class ContractTypeGridComponent extends AppInfiniteGrid<ContractTypeDto> {

    private final ListAction<String> listAction;
    private final ListAction<ContractTypeDto> listActionForChange;
    private final SaveAction<String> saveActionChange;

    public ContractTypeGridComponent(DeleteAction deleteAction, AppEnv appEnv, ItemsAction<ContractTypeDto> itemsAction,
                                     ListAction<String> listAction, ListAction<ContractTypeDto> listActionForChange,
                                     SaveAction<String> saveActionChange) {
        super(deleteAction, appEnv, itemsAction);
        this.listAction = listAction;
        this.listActionForChange = listActionForChange;
        this.saveActionChange = saveActionChange;
        initGrid();
    }

    private void initGrid() {
        addColumn(ContractTypeDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(ContractTypeDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        addColumn(new ComponentRenderer<>(contractTypeDto -> VaadinComponents
                .getCheckUncheckLayoutNullFalse(contractTypeDto.getSales()))).setHeader(Transl.get("Sales"))
                .setWidth(VaadinValues.COLUMN_ICON_SIZE_MEDIUM).setFlexGrow(0).setSortable(true).setKey("sales");
        addColumn(new ComponentRenderer<>(contractTypeDto -> VaadinComponents
                .getCheckUncheckLayoutNullFalse(contractTypeDto.getOperational()))).setHeader(Transl.get("Operational"))
                .setWidth(VaadinValues.COLUMN_ICON_SIZE_MEDIUM).setFlexGrow(0).setSortable(true).setKey("operational");
        addColumn(new ComponentRenderer<>(contractTypeDto -> VaadinComponents
                .getCheckUncheckLayoutNullFalse(contractTypeDto.getSupplierCo()))).setHeader(Transl.get("Supplier co."))
                .setWidth(VaadinValues.COLUMN_ICON_SIZE_MEDIUM).setFlexGrow(0).setSortable(true).setKey("supplierCo");
        addColumn(new ComponentRenderer<>(contracttypeDto -> VaadinComponents
                .getCheckUncheckLayoutNullFalse(contracttypeDto.getEmployeeCo()))).setHeader(Transl.get("Employee co."))
                .setWidth(VaadinValues.COLUMN_ICON_SIZE_MEDIUM).setFlexGrow(0).setSortable(true).setKey("employeeCo");
        addColumn(new ComponentRenderer<>(contractTypeDto -> VaadinComponents
                .getCheckUncheckLayoutNullTrue(contractTypeDto.getAllowed())))
                .setHeader(Transl.get("Allowed"))
                .setWidth(VaadinValues.COLUMN_ICON_SIZE_MEDIUM)
                .setFlexGrow(0)
                .setSortable(true).setKey("allowed");
        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));
    }

    private HorizontalLayout getGridButtons(ContractTypeDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");

        if (SecurityUtils.hasPermission(Permission.CONTRACT_TYPE_DELETE)) {
            Button delete = VaadinComponents.getDeleteButton();
            AppUtils.addRfClassToGridButton(delete, clickedItem.getId());
            delete.addClickListener(buttonClickEvent -> {
                List<String> contractList = listAction.getList(clickedItem.getId());
                if (contractList.isEmpty()) {
                    DeleteConfirmDialog deleteConfirmDialog =
                            new DeleteConfirmDialog(this, clickedItem.getId(), Transl.get(
                                    "Are you sure you want to delete contract type {0} ",
                                    clickedItem.getId()
                            ), getAppEnv(), true);
                    deleteConfirmDialog.open();
                } else {
                    ChangeWarningDialog warningListDialog = getChangeWarningDialog(clickedItem, contractList);
                    warningListDialog.open();
                }
            });
            delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete contract type"));
            buttons.add(delete);
        }
        return buttons;
    }

    private ChangeWarningDialog getChangeWarningDialog(ContractTypeDto clickedItem, List<String> contractList) {
        ComboBox<ContractTypeDto> contractTypes = new ComboBox<>(Transl.get("Change type to"));
        contractTypes.setItems(listActionForChange.getList(clickedItem.getId()));
        contractTypes.setItemLabelGenerator(ContractTypeDto::getName);
        Binder<ChangeContractTypeDto> binder = new Binder<>();
        binder.forField(contractTypes).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY)).
                bind(ChangeContractTypeDto::getContractTypeDto, ChangeContractTypeDto::setContractTypeDto);
        binder.setBean(new ChangeContractTypeDto());

        Button changeButton = VaadinComponents.getChangeButton(Transl.get("Change and delete"));

        ChangeWarningDialog warningDialog = new ChangeWarningDialog(contractList, contractTypes, changeButton,
                "Delete contract type", "The contract type is used", "Contracts");

        changeButton.setDisableOnClick(true);
        changeButton.addClickListener(buttonClickEvent -> {
            if (binder.validate().isOk()) {
                saveActionChange.saveItem(contractTypes.getValue().getId(), clickedItem.getId());
                this.loadData();
                warningDialog.close();
            }
            changeButton.setEnabled(true);
        });
        return warningDialog;
    }

    private void gridClicked(String code) {
        UI.getCurrent().navigate(ContractTypeDetailView.ROUTE + "/" + code);
    }
}
