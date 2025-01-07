package cz.bbn.cerberus.dph.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.dph.DphComponentOperation;
import cz.bbn.cerberus.dph.dto.DphDto;
import cz.bbn.cerberus.dph.ui.component.tab.DphDetailTabComponent;
import cz.bbn.cerberus.translation.Transl;

public class DphNewDialog extends AppDialog implements AppBinderOperation<DphDto> {

    private final AppInfiniteGrid<DphDto> grid;
    private final DphComponentOperation dphComponentOperation;
    private final AppEnv appEnv;

    private final Binder<DphDto> binder = new Binder<>();
    private final DphDto dto = new DphDto();

    public DphNewDialog(AppInfiniteGrid<DphDto> grid, DphComponentOperation dphComponentOperation, AppEnv appEnv) {
        this.grid = grid;
        this.dphComponentOperation = dphComponentOperation;
        this.appEnv = appEnv;
        init();
    }

    void init() {
        setTitle(Transl.get("New DPH"));

        DphDetailTabComponent dphDetailTabComponent =
                new DphDetailTabComponent(this, true);
        setContent(dphDetailTabComponent);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            if (binder.validate().isOk()) {
                dphComponentOperation.getSaveAction(this).saveItem(dto, new DphDto());
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
    public Binder<DphDto> getBinder() {
        return binder;
    }

    @Override
    public DphDto getDto() {
        return dto;
    }
}
