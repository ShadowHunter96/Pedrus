package cz.bbn.cerberus.technology.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.technology.TechnologyComponentOperation;
import cz.bbn.cerberus.technology.dto.TechnologyDto;
import cz.bbn.cerberus.technology.ui.component.tab.TechnologyDetailTabComponent;
import cz.bbn.cerberus.translation.Transl;

public class TechnologyNewDialog extends AppDialog implements AppBinderOperation<TechnologyDto> {

    private final AppInfiniteGrid<TechnologyDto> grid;
    private final TechnologyComponentOperation technologyComponentOperation;
    private final AppEnv appEnv;

    private final Binder<TechnologyDto> binder = new Binder<>();
    private final TechnologyDto dto = new TechnologyDto();

    public TechnologyNewDialog(AppInfiniteGrid<TechnologyDto> grid,
                               TechnologyComponentOperation technologyComponentOperation, AppEnv appEnv) {
        this.grid = grid;
        this.technologyComponentOperation = technologyComponentOperation;
        this.appEnv = appEnv;
        init();
    }

    void init() {
        setTitle(Transl.get("New technology"));

        TechnologyDetailTabComponent assetPositionDetailTabComponent =
                new TechnologyDetailTabComponent(this, true);
        setContent(assetPositionDetailTabComponent);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            if (binder.validate().isOk()) {
                technologyComponentOperation.getSaveAction(this).saveItem(dto, new TechnologyDto());
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
    public Binder<TechnologyDto> getBinder() {
        return binder;
    }

    @Override
    public TechnologyDto getDto() {
        return dto;
    }
}
