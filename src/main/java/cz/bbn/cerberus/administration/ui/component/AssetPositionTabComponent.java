package cz.bbn.cerberus.administration.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.assetposition.AssetPositionComponentOperation;
import cz.bbn.cerberus.assetposition.AssetPositionService;
import cz.bbn.cerberus.assetposition.dto.AssetPositionDto;
import cz.bbn.cerberus.assetposition.dto.AssetPositionFilterDto;
import cz.bbn.cerberus.assetposition.ui.component.AssetPositionFilterComponent;
import cz.bbn.cerberus.assetposition.ui.component.AssetPositionGridComponent;
import cz.bbn.cerberus.assetposition.ui.component.AssetPositionNewDialog;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class AssetPositionTabComponent extends TabSimpleComponent {

    public static final int TAB_INDEX = 4;

    private final AssetPositionService assetPositionService;
    private final AssetPositionComponentOperation assetPositionComponentOperation;
    private final AppEnv appEnv;

    private AssetPositionGridComponent assetPositionGridComponent;

    public AssetPositionTabComponent(AppEnv appEnv, AssetPositionService assetPositionService,
                                     AssetPositionComponentOperation assetPositionComponentOperation) {
        this.assetPositionService = assetPositionService;
        this.assetPositionComponentOperation = assetPositionComponentOperation;
        this.appEnv = appEnv;
        initComponent();
    }

    private void initComponent() {
        setSizeFull();
        setId(RobotFrameworkVariables.ASSET_POSITION_TAB_CARD_ID.getValue());

        Button search = VaadinComponents.getSearchButton();
        AssetPositionFilterComponent assetPositionFilterComponent = new AssetPositionFilterComponent(search);
        this.add(assetPositionFilterComponent);

        assetPositionGridComponent =
                new AssetPositionGridComponent(getDeleteAction(), appEnv, getItemsAction(assetPositionFilterComponent),
                        assetPositionComponentOperation);

        assetPositionGridComponent.loadData();
        search.addClickListener(buttonClickEvent -> assetPositionGridComponent.loadData());
        this.add(assetPositionGridComponent);

    }

    @Override
    public Button getFooterButton() {
        if (SecurityUtils.hasPermission(Permission.ASSET_POSITION_EDIT)) {
            Button addNew = VaadinComponents.getNewButton(Transl.get("Add asset position"));
            addNew.addClickListener(e -> new AssetPositionNewDialog(
                    assetPositionGridComponent, assetPositionComponentOperation, appEnv).open());
            return addNew;
        }
        return null;
    }

    private DeleteAction getDeleteAction() {
        return id -> {
            try {
                assetPositionService.delete(id);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    private ItemsAction<AssetPositionDto> getItemsAction(AssetPositionFilterComponent assetPositionFilterComponent) {
        return (query, orderList) -> {
            AssetPositionFilterDto filter = assetPositionFilterComponent.getAssetPositionFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return assetPositionService.findAssetPositionDtoPage(filter);

        };
    }
}
