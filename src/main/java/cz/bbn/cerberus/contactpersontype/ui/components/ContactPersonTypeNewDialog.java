package cz.bbn.cerberus.contactpersontype.ui.components;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactpersontype.ContactPersonTypeComponentOperation;
import cz.bbn.cerberus.contactpersontype.dto.ContactPersonTypeDto;
import cz.bbn.cerberus.contactpersontype.ui.components.tab.ContactPersonTypeDetailTabComponent;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class ContactPersonTypeNewDialog extends AppDialog implements AppBinderOperation<ContactPersonTypeDto> {

    private final AppInfiniteGrid<ContactPersonTypeDto> grid;
    private final ContactPersonTypeComponentOperation contactPersonTypeComponentOperation;

    private final Binder<ContactPersonTypeDto> binder = new Binder<>();
    private final ContactPersonTypeDto dto = new ContactPersonTypeDto();

    public ContactPersonTypeNewDialog(AppInfiniteGrid<ContactPersonTypeDto> grid,
                                      ContactPersonTypeComponentOperation contactPersonTypeComponentOperation) {
        this.grid = grid;
        this.contactPersonTypeComponentOperation = contactPersonTypeComponentOperation;
        init();
    }

    void init() {
        setTitle(Transl.get("New contact person type"));

        ContactPersonTypeDetailTabComponent contactPersonTypeDetailTabComponent =
                new ContactPersonTypeDetailTabComponent(this, true);
        setContent(contactPersonTypeDetailTabComponent);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            if (binder.validate().isOk()) {
                contactPersonTypeComponentOperation.getSaveAction(this).saveItem(dto, null);
                if (grid != null) {
                    grid.loadData();
                }
            } else {
                ErrorNotification.show(Transl.get(TextValues.INVALIS_INPUT),
                        contactPersonTypeComponentOperation.getAppEnv());
            }
            submit.setEnabled(true);
        });

        showWarning(true);

        addCloseButton();
        if (SecurityUtils.hasPermission(Permission.CONTACT_PERSON_TYPE_EDIT)) {
            addButtons(submit);
        }
    }

    @Override
    public Binder<ContactPersonTypeDto> getBinder() {
        return binder;
    }

    @Override
    public ContactPersonTypeDto getDto() {
        return dto;
    }
}
