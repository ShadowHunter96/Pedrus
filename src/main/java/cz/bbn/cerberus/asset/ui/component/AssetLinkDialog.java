package cz.bbn.cerberus.asset.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.asset.AssetComponentOperation;
import cz.bbn.cerberus.asset.dto.AssetByObjectDto;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;

public class AssetLinkDialog extends AppDialog {

    private final String title;
    private final AssetComponentOperation assetComponentOperation;
    private final AppEnv appEnv;
    private final String objectId;
    private final ObjectType objectType;
    private final AppInfiniteGrid<AssetByObjectDto> assetByObjectGrid;
    private final AssetFilterComponent filter;

    private AssetGridComponent assetGridComponent;

    public AssetLinkDialog(String title, AssetComponentOperation assetComponentOperation, AppEnv appEnv,
                           String objectId, ObjectType objectType, AppInfiniteGrid<AssetByObjectDto> assetByObjectGrid,
                           AssetFilterComponent filter) {
        this.title = title;
        this.assetComponentOperation = assetComponentOperation;
        this.appEnv = appEnv;
        this.objectId = objectId;
        this.objectType = objectType;
        this.assetByObjectGrid = assetByObjectGrid;
        this.filter = filter;
        initComponent();
    }

    private void initComponent() {
        removeAll();
        setTitle(title);
        Button searchButton = VaadinComponents.getSearchButton();
        assetGridComponent = new AssetGridComponent(assetComponentOperation.getLinkItemsAction(filter, objectId),
                appEnv, assetComponentOperation);
        assetGridComponent.loadData();
        searchButton.addClickListener(e -> assetGridComponent.loadData());
        assetGridComponent.allowMultiselect();

        setContent(filter, assetGridComponent);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(buttonClickEvent -> {
            assetComponentOperation.linkAsset(assetGridComponent.getSelectedItems(), objectId, objectType);
            this.close();
            this.assetByObjectGrid.loadData();
            submit.setEnabled(true);
        });

        addCloseButton();
        addSubmitButton(submit);
    }

    public void loadData() {
        assetGridComponent.loadData();
    }
}
