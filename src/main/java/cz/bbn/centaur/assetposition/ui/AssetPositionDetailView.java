package cz.bbn.cerberus.assetposition.ui;

import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.assetposition.AssetPositionComponentOperation;
import cz.bbn.cerberus.assetposition.AssetPositionService;
import cz.bbn.cerberus.assetposition.dto.AssetPositionDto;
import cz.bbn.cerberus.assetposition.ui.component.AssetPositionDetailComponent;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import lombok.extern.slf4j.Slf4j;

@Route(value = AssetPositionDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.ASSET_POSITION_VIEW)
@Slf4j
public class AssetPositionDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "asset-position-detail";

    private final AssetPositionService assetPositionService;
    private final AssetPositionComponentOperation assetPositionComponentOperation;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public AssetPositionDetailView(AssetPositionService assetPositionService,
                                   AssetPositionComponentOperation assetPositionComponentOperation, AppEnv appEnv,
                                   EntityNewComponentOperation entityNewComponentOperation) {
        this.assetPositionService = assetPositionService;
        this.assetPositionComponentOperation = assetPositionComponentOperation;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(AssetPositionDto assetPositionDto) {
        AssetPositionDetailComponent assetPositionDetailComponent =
                new AssetPositionDetailComponent(assetPositionDto, assetPositionComponentOperation.getSaveAction(null),
                        SecurityUtils.hasPermission(Permission.ASSET_POSITION_EDIT),
                        appEnv, entityNewComponentOperation);
        this.add(assetPositionDetailComponent);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String param) {
        AssetPositionDto dto = new AssetPositionDto();
        if (param != null) {
            try {
                dto = assetPositionService.getAssetPosition(param);
                refreshBreadcrumbText(dto.getId());
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        }
        initView(dto);
    }
}
