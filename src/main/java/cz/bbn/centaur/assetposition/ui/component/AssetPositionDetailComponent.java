package cz.bbn.cerberus.assetposition.ui.component;

import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.AssetPositionTabComponent;
import cz.bbn.cerberus.assetposition.dto.AssetPositionDto;
import cz.bbn.cerberus.assetposition.ui.component.tab.AssetPositionDetailTabComponent;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.appcard.AppDetailCardComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.translation.Transl;

public class AssetPositionDetailComponent
        extends AppDetailCardComponent<AssetPositionDto> implements AppBinderOperation<AssetPositionDto> {

    public AssetPositionDetailComponent(AssetPositionDto dto, SaveAction<AssetPositionDto> saveAction,
                                        boolean showSubmitButton, AppEnv appEnv,
                                        EntityNewComponentOperation entityNewComponentOperation) {
        super(dto, saveAction, showSubmitButton, appEnv, entityNewComponentOperation);
        initComponent();
    }

    @Override
    protected void initComponent() {
        String heading = getDto().getId() == null ? Transl.get("New asset position") :
                Transl.get("Asset position")
                        .concat(" - ")
                        .concat(getDto().getName());
        setHeading(heading);
        this.addBackButton(AdministrationView.ROUTE + "/" + AssetPositionTabComponent.TAB_INDEX);
        if (isShowSubmitButton()) {
            this.addSaveButton();
        }
        this.setId(RobotFrameworkVariables.ASSET_POSITION_DETAIL_CARD_ID.getValue());
        this.setSizeFull();

        this.add(new AssetPositionDetailTabComponent(this, false, getAppEnv()));
    }
}
