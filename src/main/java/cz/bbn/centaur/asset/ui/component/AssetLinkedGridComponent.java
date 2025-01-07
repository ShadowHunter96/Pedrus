package cz.bbn.cerberus.asset.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.asset.AssetComponentOperation;
import cz.bbn.cerberus.asset.dto.AssetByObjectDto;
import cz.bbn.cerberus.asset.ui.AssetDetailView;
import cz.bbn.cerberus.commons.AppGridDataVariables;
import cz.bbn.cerberus.commons.AppGridStringVariables;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.UnlinkAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class AssetLinkedGridComponent extends AppInfiniteGrid<AssetByObjectDto> {

    private final UnlinkAction unlinkAction;
    private final AssetComponentOperation assetComponentOperation;

    public AssetLinkedGridComponent(AppEnv appEnv, ItemsAction<AssetByObjectDto> itemsAction,
                                    AssetComponentOperation assetComponentOperationMap, UnlinkAction unlinkAction) {
        super(appEnv, itemsAction);
        this.unlinkAction = unlinkAction;
        this.assetComponentOperation = assetComponentOperationMap;
        initComponent();
    }

    private void initComponent() {
        setSizeFull();
        addColumn(AssetByObjectDto::getAssetId).setHeader(Transl.get("Id")).setSortable(true).setKey("assetId");
        addColumn(AssetByObjectDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        addColumn(AssetByObjectDto::getSerialNumber).setHeader(Transl.get("Serial number")).setSortable(true)
                .setKey("serialNumber");
        addColumn(new ComponentRenderer<>(this::getEmployeeName)).setHeader(Transl.get("Owner")).setSortable(true)
                .setKey("owner");
        addColumn(AssetByObjectDto::getType).setHeader(Transl.get("Type")).setSortable(true).setKey("type");
        addColumn(assetByObjectDto -> AppUtils.formatDate(assetByObjectDto.getBuyDate()))
                .setHeader(Transl.get("Buy date")).setSortable(true).setKey("buyDate");
        addColumn(assetByObjectDto -> AppUtils.priceWithDecimal(assetByObjectDto.getPrice()))
                .setHeader(Transl.get("Price"))
                .setSortable(true)
                .setKey("price");
        setColumnReorderingAllowed(true);
        setMultiSort(true);

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(1).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(event -> gridClicked(event.getItem().getAssetId()));

        loadData();
    }

    private HorizontalLayout getGridButtons(AssetByObjectDto clickedItem) {
        AppGridStringVariables stringVariables = new AppGridStringVariables("Edit asset", "",
                "Unlink asset");
        AppGridDataVariables dataVariables = new AppGridDataVariables(
                Permission.ASSET_EDIT, Permission.ASSET_EDIT, clickedItem.getAssetId(), clickedItem.getId(),
                "", AssetDetailView.ROUTE);
        return getUnlinkGridButtons(unlinkAction, dataVariables, stringVariables, this);
    }

    private void gridClicked(String code) {
        UI.getCurrent().navigate(AssetDetailView.ROUTE + "/" + code);
    }

    private Span getEmployeeName(AssetByObjectDto assetByObjectDto) {
        return new Span(assetComponentOperation.getEmployeeName(assetByObjectDto.getResponsiblePerson()));
    }
}
