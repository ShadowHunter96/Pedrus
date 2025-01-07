package cz.bbn.cerberus.area.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.area.AreaComponentOperation;
import cz.bbn.cerberus.area.dto.AreaDto;
import cz.bbn.cerberus.area.ui.component.tab.AreaDetailTabComponent;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.translation.Transl;

public class AreaNewDialog extends AppDialog implements AppBinderOperation<AreaDto> {

    private final AppInfiniteGrid<AreaDto> grid;
    private final AreaComponentOperation areaComponentOperation;
    private final AppEnv appEnv;

    private final Binder<AreaDto> binder = new Binder<>();
    private final AreaDto dto = new AreaDto();

    public AreaNewDialog(AppInfiniteGrid<AreaDto> grid, AreaComponentOperation areaComponentOperation, AppEnv appEnv) {
        this.grid = grid;
        this.areaComponentOperation = areaComponentOperation;
        this.appEnv = appEnv;
        init();
    }

    void init() {
        setTitle(Transl.get("New area"));

        AreaDetailTabComponent assetPositionDetailTabComponent =
                new AreaDetailTabComponent(this, true);
        setContent(assetPositionDetailTabComponent);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            if (binder.validate().isOk()) {
                areaComponentOperation.getSaveAction(this).saveItem(dto, new AreaDto());
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
    public Binder<AreaDto> getBinder() {
        return binder;
    }

    @Override
    public AreaDto getDto() {
        return dto;
    }
}
