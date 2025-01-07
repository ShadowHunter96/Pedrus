package cz.bbn.cerberus.asset.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.asset.AssetComponentOperation;
import cz.bbn.cerberus.asset.dto.AssetSimpleDto;
import cz.bbn.cerberus.asset.ui.AssetDetailView;
import cz.bbn.cerberus.commons.AppGridDataVariables;
import cz.bbn.cerberus.commons.AppGridStringVariables;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;


public class AssetGridComponent extends AppInfiniteGrid<AssetSimpleDto> {

    private final AssetComponentOperation assetComponentOperation;
    private final boolean simpleGrid;

    public AssetGridComponent(ItemsAction<AssetSimpleDto> itemsAction, AppEnv appEnv,
                              AssetComponentOperation assetComponentOperation) {
        super(appEnv, itemsAction);
        this.assetComponentOperation = assetComponentOperation;
        this.simpleGrid = true;
        initGrid();
    }

    public AssetGridComponent(DeleteAction deleteAction, AppEnv appEnv,
                              ItemsAction<AssetSimpleDto> itemsAction,
                              AssetComponentOperation assetComponentOperation) {
        super(deleteAction, appEnv, itemsAction);
        this.assetComponentOperation = assetComponentOperation;
        this.simpleGrid = false;
        initGrid();
    }

    private void initGrid() {
        addColumn(AssetSimpleDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(AssetSimpleDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        addColumn(AssetSimpleDto::getSerialNumber).setHeader(Transl.get("Serial number")).setSortable(true)
                .setKey("serialNumber");
        addColumn(new ComponentRenderer<>(this::getResponsiblePersonName)).setHeader(Transl.get("Owner"))
                .setSortable(true).setKey("owner");
        addColumn(AssetSimpleDto::getType).setHeader(Transl.get("Type")).setSortable(true).setKey("type");
        addColumn(assetSimpleDto -> AppUtils.formatDate(assetSimpleDto.getBuyDate())).setHeader(Transl.get("Buy date"))
                .setSortable(true).setKey("buyDate");
        addColumn(assetSimpleDto -> AppUtils.priceWithDecimal(assetSimpleDto.getPrice()))
                .setHeader(Transl.get("Price"))
                .setSortable(true)
                .setKey("price");
        setColumnReorderingAllowed(true);
        setMultiSort(true);

        if (!simpleGrid) {
            addColumn(new ComponentRenderer<>(this::getGridButtons))
                    .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                    .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                    .setFlexGrow(1).setTextAlign(ColumnTextAlign.CENTER);
        }

        addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));

    }

    private HorizontalLayout getGridButtons(AssetSimpleDto clickedItem) {
        AppGridStringVariables stringVariables = new AppGridStringVariables("Edit asset",
                "Are you sure you want to delete Asset {0} ?", "Delete asset");
        AppGridDataVariables dataVariables = new AppGridDataVariables(
                Permission.ASSET_EDIT, Permission.ASSET_DELETE, clickedItem.getId(), clickedItem.getName(),
                AssetDetailView.ROUTE, clickedItem.getDeleted());
        return getSimpleGridButtons(dataVariables, stringVariables, this);
    }

    private void gridClicked(String code) {
        UI.getCurrent().navigate(AssetDetailView.ROUTE + "/" + code);
    }

    private Span getResponsiblePersonName(AssetSimpleDto assetSimpleDto) {
        return new Span(assetComponentOperation.getEmployeeName(assetSimpleDto.getResponsiblePerson()));
    }
}
