package cz.bbn.cerberus.enumeration.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.enumeration.EnumerationComponentOperation;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationTypeDto;
import cz.bbn.cerberus.translation.Transl;

public class EnumerationNewDialog extends AppDialog implements AppBinderOperation<EnumerationDto> {

    private final EnumerationComponentOperation enumerationComponentOperation;
    private final EnumerationGridComponent grid;
    private final EnumerationTypeDto enumerationTypeDto;
    private final AppEnv appEnv;
    private final int tabIndex;

    private final Binder<EnumerationDto> binder = new Binder<>();
    private final EnumerationDto dto = new EnumerationDto();

    public EnumerationNewDialog(EnumerationComponentOperation enumerationComponentOperation,
                                EnumerationGridComponent grid,
                                EnumerationTypeDto enumerationTypeDto, AppEnv appEnv, int tabIndex) {
        this.enumerationComponentOperation = enumerationComponentOperation;
        this.grid = grid;
        this.enumerationTypeDto = enumerationTypeDto;
        this.appEnv = appEnv;
        this.tabIndex = tabIndex;
        initComponent();
    }

    void initComponent() {
        setTitle(Transl.get("New ".concat(enumerationTypeDto.getTranslationKey())));

        dto.setAllowed(true);
        dto.setEnumerationTypeDto(enumerationTypeDto);
        dto.setValue("false");

        EnumerationDetailTabComponent enumerationDetailTabComponent =
                new EnumerationDetailTabComponent(this, true, enumerationComponentOperation);
        setContent(enumerationDetailTabComponent);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            if (binder.validate().isOk()) {
                enumerationComponentOperation.getSaveAction(this, tabIndex).saveItem(dto, null);
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
    public Binder<EnumerationDto> getBinder() {
        return binder;
    }

    @Override
    public EnumerationDto getDto() {
        return dto;
    }
}
