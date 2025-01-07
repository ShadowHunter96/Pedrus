package cz.bbn.cerberus.contactpersontype;

import com.vaadin.flow.component.UI;
import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.ContactPersonTypeTabComponent;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.contactpersontype.dto.ContactPersonTypeDto;
import cz.bbn.cerberus.contactpersontype.ui.ContactPersonTypeDetailView;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class ContactPersonTypeComponentOperation {

    private final ContactPersonTypeService contactPersonTypeService;
    private final AppEnv appEnv;

    public ContactPersonTypeComponentOperation(ContactPersonTypeService contactPersonTypeService, AppEnv appEnv) {
        this.contactPersonTypeService = contactPersonTypeService;
        this.appEnv = appEnv;
    }

    public SaveAction<ContactPersonTypeDto> getSaveAction(AppDialog appDialog) {
        return (dto, originalDto) -> {
            try {
                if (contactPersonTypeService.contactPersonTypeExists(dto.getId())) {
                    contactPersonTypeService.updateContactTypePerson(dto, originalDto);
                    UI.getCurrent().navigate(AdministrationView.ROUTE.concat("/").concat(
                            String.valueOf(ContactPersonTypeTabComponent.TAB_INDEX)));
                } else {
                    contactPersonTypeService.saveContactPersonType(dto);
                    if (appDialog != null) {
                        appDialog.showWarning(false);
                        appDialog.close();
                        UI.getCurrent().navigate(ContactPersonTypeDetailView.ROUTE.concat("/").concat(dto.getId()));
                    }
                }
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public AppEnv getAppEnv() {
        return appEnv;
    }
}
