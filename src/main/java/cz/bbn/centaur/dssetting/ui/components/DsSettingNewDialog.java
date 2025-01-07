package cz.bbn.cerberus.dssetting.ui.components;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.dssetting.DsSettingComponentOperation;
import cz.bbn.cerberus.dssetting.dto.DsSettingDto;
import cz.bbn.cerberus.dssetting.dto.DsSettingSimpleDto;
import cz.bbn.cerberus.dssetting.ui.components.tab.DsSettingDetailTabComponent;
import cz.bbn.cerberus.translation.Transl;

public class DsSettingNewDialog extends AppDialog implements AppBinderOperation<DsSettingDto> {

    private final AppInfiniteGrid<DsSettingSimpleDto> grid;
    private final DsSettingComponentOperation dsSettingComponentOperation;

    private final Binder<DsSettingDto> binder = new Binder<>();
    private final DsSettingDto dto = new DsSettingDto();

    public DsSettingNewDialog(AppInfiniteGrid<DsSettingSimpleDto> grid,
                              DsSettingComponentOperation dsSettingComponentOperation) {
        this.grid = grid;
        this.dsSettingComponentOperation = dsSettingComponentOperation;
        init();
    }

    void init() {
        setTitle(Transl.get("New ds setting"));

        DsSettingDetailTabComponent dsSettingDetailTabComponent = new DsSettingDetailTabComponent(this,
                dsSettingComponentOperation.getUserList(), true, dsSettingComponentOperation.getAppEnv(), false);
        setContent(dsSettingDetailTabComponent);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            if (binder.validate().isOk()) {
                dsSettingComponentOperation.getSaveAction(this).saveItem(dto, new DsSettingDto());
                if (grid != null) {
                    grid.loadData();
                }
            } else {
                ErrorNotification.show(Transl.get(TextValues.INVALIS_INPUT), dsSettingComponentOperation.getAppEnv());
            }
            submit.setEnabled(true);
        });

        showWarning(true);

        addCloseButton();
        addButtons(submit);
    }

    @Override
    public Binder<DsSettingDto> getBinder() {
        return binder;
    }

    @Override
    public DsSettingDto getDto() {
        return dto;
    }
}
