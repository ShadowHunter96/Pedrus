package cz.bbn.cerberus.assetposition.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.assetposition.AssetPositionComponentOperation;
import cz.bbn.cerberus.assetposition.dto.AssetPositionDto;
import cz.bbn.cerberus.assetposition.ui.AssetPositionDetailView;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.DeleteConfirmDialog;
import cz.bbn.cerberus.commons.component.ui.dialog.WarningListDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class AssetPositionGridComponent extends AppInfiniteGrid<AssetPositionDto> {

    private final AssetPositionComponentOperation assetPositionComponentOperation;

    public AssetPositionGridComponent(DeleteAction deleteAction, AppEnv appEnv,
                                      ItemsAction<AssetPositionDto> itemsAction,
                                      AssetPositionComponentOperation assetPositionComponentOperation) {
        super(deleteAction, appEnv, itemsAction);
        this.assetPositionComponentOperation = assetPositionComponentOperation;
        initGrid();
    }

    private void initGrid() {
        addColumn(AssetPositionDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(AssetPositionDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));
    }

    private HorizontalLayout getGridButtons(AssetPositionDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");

        if (SecurityUtils.hasPermission(Permission.ASSET_POSITION_DELETE)) {
            Button delete = VaadinComponents.getDeleteButton();
            AppUtils.addRfClassToGridButton(delete, clickedItem.getId());
            delete.addClickListener(buttonClickEvent -> {
                List<String> usedInAssetTextList =
                        assetPositionComponentOperation.getUsedInAssetTextList(clickedItem.getId());
                if (usedInAssetTextList.isEmpty()) {
                    DeleteConfirmDialog deleteConfirmDialog =
                            new DeleteConfirmDialog(this, clickedItem.getId(),
                                    Transl.get("Are you sure you want to delete asset position {0} ",
                                            clickedItem.getId()), getAppEnv(), true);
                    deleteConfirmDialog.open();
                } else {
                    WarningListDialog warningListDialog = new WarningListDialog(usedInAssetTextList,
                            Transl.get("Asset position cannot be deleted"), Transl.get("Asset position is assigned to"),
                            Transl.get("Assets").concat(":"));
                    warningListDialog.open();
                }
            });
            delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete asset position"));
            buttons.add(delete);
        }

        return buttons;
    }

    private void gridClicked(String code) {
        UI.getCurrent().navigate(AssetPositionDetailView.ROUTE + "/" + code);
    }
}
