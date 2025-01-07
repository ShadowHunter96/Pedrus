package cz.bbn.cerberus.contracttype.ui.components;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.contracttype.ContractTypeComponentOperation;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.contracttype.ui.components.tab.ContractTypeDetailTabComponent;
import cz.bbn.cerberus.translation.Transl;

public class ContractTypeNewDialog extends AppDialog implements AppBinderOperation<ContractTypeDto> {

    private final AppInfiniteGrid<ContractTypeDto> grid;
    private final ContractTypeComponentOperation contractTypeComponentOperation;

    private final Binder<ContractTypeDto> binder = new Binder<>();
    private final ContractTypeDto dto = new ContractTypeDto();

    public ContractTypeNewDialog(AppInfiniteGrid<ContractTypeDto> grid,
                                 ContractTypeComponentOperation contractTypeComponentOperation) {
        this.grid = grid;
        this.contractTypeComponentOperation = contractTypeComponentOperation;
        init();
    }

    void init() {
        setTitle(Transl.get("New contract type"));

        ContractTypeDetailTabComponent contractTypeDetailTabComponent =
                new ContractTypeDetailTabComponent(this, true);
        setContent(contractTypeDetailTabComponent);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            if (binder.validate().isOk()) {
                contractTypeComponentOperation.getSaveAction(this).saveItem(dto, null);
                if (grid != null) {
                    grid.loadData();
                }
            } else {
                ErrorNotification.show(Transl.get(TextValues.INVALIS_INPUT),
                        contractTypeComponentOperation.getAppEnv());
            }
            submit.setEnabled(true);
        });

        showWarning(true);

        addCloseButton();
        addButtons(submit);
    }

    @Override
    public Binder<ContractTypeDto> getBinder() {
        return binder;
    }

    @Override
    public ContractTypeDto getDto() {
        return dto;
    }
}
