package cz.bbn.cerberus.suppliertype.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.suppliertype.SupplierTypeComponentOperation;
import cz.bbn.cerberus.suppliertype.dto.SupplierTypeDto;
import cz.bbn.cerberus.suppliertype.ui.component.tab.SupplierTypeDetailTabComponent;
import cz.bbn.cerberus.translation.Transl;

public class SupplierTypeNewDialog extends AppDialog implements AppBinderOperation<SupplierTypeDto> {

    private final AppInfiniteGrid<SupplierTypeDto> grid;
    private final SupplierTypeComponentOperation supplierTypeComponentOperation;

    private final Binder<SupplierTypeDto> binder = new Binder<>();
    private final SupplierTypeDto dto = new SupplierTypeDto();

    public SupplierTypeNewDialog(AppInfiniteGrid<SupplierTypeDto> grid,
                                 SupplierTypeComponentOperation supplierTypeComponentOperation) {
        this.grid = grid;
        this.supplierTypeComponentOperation = supplierTypeComponentOperation;
        init();
    }

    void init() {
        setTitle(Transl.get("New supplier type"));

        SupplierTypeDetailTabComponent supplierTypeDetailTabComponent =
                new SupplierTypeDetailTabComponent(this, true);
        setContent(supplierTypeDetailTabComponent);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            if (binder.validate().isOk()) {
                supplierTypeComponentOperation.getSaveAction(this).saveItem(dto, null);
                if (grid != null) {
                    grid.loadData();
                }
            } else {
                ErrorNotification.show(Transl.get(TextValues.INVALIS_INPUT),
                        supplierTypeComponentOperation.getAppEnv());
            }
            submit.setEnabled(true);
        });

        showWarning(true);

        addCloseButton();
        addButtons(submit);
    }

    @Override
    public Binder<SupplierTypeDto> getBinder() {
        return binder;
    }

    @Override
    public SupplierTypeDto getDto() {
        return dto;
    }
}
