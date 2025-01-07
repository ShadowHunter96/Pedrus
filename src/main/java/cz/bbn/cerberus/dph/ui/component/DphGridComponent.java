package cz.bbn.cerberus.dph.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppGridDataVariables;
import cz.bbn.cerberus.commons.AppGridStringVariables;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.dph.DphComponentOperation;
import cz.bbn.cerberus.dph.dto.DphDto;
import cz.bbn.cerberus.dph.ui.DphDetailView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class DphGridComponent extends AppInfiniteGrid<DphDto> {

    private final boolean showButtons;
    private final DphComponentOperation dphComponentOperation;

    public DphGridComponent(DeleteAction deleteAction, AppEnv appEnv, ItemsAction<DphDto> itemsAction,
                            DphComponentOperation dphComponentOperation) {
        super(deleteAction, appEnv, itemsAction);
        showButtons = true;
        this.dphComponentOperation = dphComponentOperation;
        initGrid();
    }

    private void initGrid() {
        addColumn(DphDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(DphDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        addColumn(new ComponentRenderer<>(contactPersonTypeDto ->
                VaadinComponents.getCheckUncheckLayoutNullTrue(contactPersonTypeDto.getAllowed())))
                .setHeader(Transl.get("Allowed"))
                .setWidth(VaadinValues.COLUMN_ICON_SIZE_MEDIUM)
                .setFlexGrow(0)
                .setSortable(true).setKey("allowed");
        if (showButtons) {
            addColumn(new ComponentRenderer<>(this::getGridButtons))
                    .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                    .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                    .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

            addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));
        }
    }

    private HorizontalLayout getGridButtons(DphDto clickedItem) {
        AppGridDataVariables appGridDataVariables = new AppGridDataVariables(Permission.DPH_EDIT, Permission.DPH_DELETE,
                clickedItem.getId(), clickedItem.getName(), DphDetailView.ROUTE, !clickedItem.getAllowed());
        AppGridStringVariables appGridStringVariables = new AppGridStringVariables("Edit VAT",
                "Are you sure you want to delete VAT {0} ?", "VAT cannot be deleted", "VAT is assigned to",
                "VAT", "Delete VAT");

        return getGridButtons(appGridDataVariables, appGridStringVariables,
                dphComponentOperation.getInvoiceListAction(), this);
    }

    private void gridClicked(String code) {
        UI.getCurrent().navigate(DphDetailView.ROUTE + "/" + code);
    }
}
