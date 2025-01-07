package cz.bbn.cerberus.assetposition;

import com.vaadin.flow.component.UI;
import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.AssetPositionTabComponent;
import cz.bbn.cerberus.asset.AssetService;
import cz.bbn.cerberus.asset.dto.AssetDto;
import cz.bbn.cerberus.assetposition.dto.AssetPositionDto;
import cz.bbn.cerberus.assetposition.ui.AssetPositionDetailView;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.listconfiguration.ListService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Component
@Slf4j
public class AssetPositionComponentOperation {

    private final AssetPositionService assetPositionService;
    private final AssetService assetService;
    private final ListService listService;
    private final AppEnv appEnv;

    public AssetPositionComponentOperation(AssetPositionService assetPositionService, AssetService assetService,
                                           ListService listService, AppEnv appEnv) {
        this.assetPositionService = assetPositionService;
        this.assetService = assetService;
        this.listService = listService;
        this.appEnv = appEnv;
    }

    public SaveAction<AssetPositionDto> getSaveAction(AppDialog appDialog) {
        return (newDto, originalDto) -> {
            try {
                if (originalDto.getId() != null) {
                    assetPositionService.updatePositionAsset(newDto, originalDto);
                    UI.getCurrent().navigate(AdministrationView.ROUTE.concat("/")
                            .concat(String.valueOf(AssetPositionTabComponent.TAB_INDEX)));
                } else {
                    newDto.setDeleted(Boolean.FALSE);
                    assetPositionService.savePositionAsset(newDto);
                    if (appDialog != null) {
                        appDialog.showWarning(false);
                        appDialog.close();
                        UI.getCurrent().navigate(AssetPositionDetailView.ROUTE.concat("/").concat(newDto.getId()));
                    }
                }
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public List<String> getUsedInAssetTextList(String id) {
        List<String> usedInAssetTextList = new ArrayList<>();
        List<String> assetIdList = assetService.getAssetIdInUseByPosition(id);
        Map<String, AssetDto> assetMap = listService.getAssetMap();
        for (String assetId : assetIdList) {
            if (assetMap.containsKey(assetId)) {
                AssetDto assetDto = assetMap.get(assetId);
                usedInAssetTextList.add(assetDto.getName().concat(" (").concat(assetDto.getId()).concat(")"));
            }
        }
        return usedInAssetTextList;
    }
}
