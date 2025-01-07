package cz.bbn.cerberus.assetposition.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.assetposition.AssetPositionComponentOperation;
import cz.bbn.cerberus.assetposition.dto.AssetPositionDto;
import cz.bbn.cerberus.assetposition.ui.component.tab.AssetPositionDetailTabComponent;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.translation.Transl;

public class AssetPositionNewDialog extends AppDialog implements AppBinderOperation<AssetPositionDto> {

    private final AppInfiniteGrid<AssetPositionDto> grid;
    private final AssetPositionComponentOperation assetPositionComponentOperation;
    private final AppEnv appEnv;

    private final Binder<AssetPositionDto> binder = new Binder<>();
    private final AssetPositionDto dto = new AssetPositionDto();

    public AssetPositionNewDialog(AppInfiniteGrid<AssetPositionDto> grid,
                                  AssetPositionComponentOperation assetPositionComponentOperation, AppEnv appEnv) {
        this.grid = grid;
        this.assetPositionComponentOperation = assetPositionComponentOperation;
        this.appEnv = appEnv;
        init();
    }

    void init() {
        setTitle(Transl.get("New contact person position"));

        AssetPositionDetailTabComponent assetPositionDetailTabComponent =
                new AssetPositionDetailTabComponent(this, true, appEnv);
        setContent(assetPositionDetailTabComponent);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            if (binder.validate().isOk()) {
                assetPositionComponentOperation.getSaveAction(this).saveItem(dto, new AssetPositionDto());
                if (grid != null) {
                    grid.loadData();
                }
            } else {
                ErrorNotification.show(Transl.get(TextValues.INVALIS_INPUT), appEnv);
            }
            submit.setEnabled(true);
        });

        showWarning(true);

        addCloseButton();
        addButtons(submit);
    }

    @Override
    public Binder<AssetPositionDto> getBinder() {
        return binder;
    }

    @Override
    public AssetPositionDto getDto() {
        return dto;
    }
}
