package cz.bbn.cerberus.asset.ui.component.tab;

import cz.bbn.cerberus.asset.AssetComponentOperation;
import cz.bbn.cerberus.asset.dto.AssetByObjectDto;
import cz.bbn.cerberus.asset.ui.component.AssetLinkedGridComponent;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.UnlinkAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;

public class AssetConnectedTab extends TabSimpleComponent {

    private final AppEnv appEnv;
    private final ItemsAction<AssetByObjectDto> itemsAction;
    private final AssetComponentOperation assetComponentOperation;
    private final UnlinkAction unlinkAction;

    private AppInfiniteGrid<AssetByObjectDto> grid;

    public AssetConnectedTab(AppEnv appEnv, ItemsAction<AssetByObjectDto> itemsAction,
                             AssetComponentOperation assetComponentOperation, UnlinkAction unlinkAction) {
        this.appEnv = appEnv;
        this.itemsAction = itemsAction;
        this.assetComponentOperation = assetComponentOperation;
        this.unlinkAction = unlinkAction;
        initTab();
    }

    private void initTab() {
        removeAll();
        setSizeFull();
        AssetLinkedGridComponent assetGrid =
                new AssetLinkedGridComponent(appEnv, itemsAction, assetComponentOperation, unlinkAction);
        add(assetGrid);
        this.grid = assetGrid;
    }

    public void loadData() {
        grid.loadData();
    }

    public AppInfiniteGrid<AssetByObjectDto> getGrid() {
        return grid;
    }
}
