package cz.bbn.cerberus.documenttype;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.checkbox.CheckboxGroup;
import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.DocumentTypeTabComponent;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.documenttype.dto.DocumentTypeDto;
import cz.bbn.cerberus.documenttype.ui.DocumentTypeDetailView;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class DocumentTypeComponentOperation {

    private final DocumentTypeService documentTypeService;
    private final AppEnv appEnv;

    public DocumentTypeComponentOperation(DocumentTypeService documentTypeService, AppEnv appEnv) {
        this.documentTypeService = documentTypeService;
        this.appEnv = appEnv;
    }

    public SaveAction<DocumentTypeDto> getSaveAction(AppDialog dialog, CheckboxGroup<EnumerationDto> checkboxGroup) {
        return (dto, originalDto) -> {
            try {
                dto.setAllowedFormatsList(checkboxGroup
                        .getSelectedItems()
                        .stream()
                        .map(EnumerationDto::getId)
                        .toList());
                if (documentTypeService.documentTypeExists(dto.getId())) {
                    documentTypeService.updateDocumentType(dto, originalDto);
                    UI.getCurrent().navigate(AdministrationView.ROUTE.concat("/")
                            .concat(String.valueOf(DocumentTypeTabComponent.TAB_INDEX))
                    );
                } else {
                    documentTypeService.saveDocumentType(dto);
                    if (dialog != null) {
                        dialog.showWarning(false);
                        dialog.close();
                        UI.getCurrent().navigate(DocumentTypeDetailView.ROUTE.concat("/").concat(dto.getId()));
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
