package cz.bbn.cerberus.documenttype.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.CheckboxGroup;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.documenttype.DocumentTypeComponentOperation;
import cz.bbn.cerberus.documenttype.dto.DocumentTypeDto;
import cz.bbn.cerberus.documenttype.ui.component.tab.DocumentTypeDetailTabComponent;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class DocumentTypeNewDialog extends AppDialog implements AppBinderOperation<DocumentTypeDto> {

    private final AppInfiniteGrid<DocumentTypeDto> grid;
    private final DocumentTypeComponentOperation documentTypeComponentOperation;

    private final Binder<DocumentTypeDto> binder = new Binder<>();
    private final DocumentTypeDto dto = new DocumentTypeDto();
    private final ListService listService;

    public DocumentTypeNewDialog(AppInfiniteGrid<DocumentTypeDto> grid,
                                 DocumentTypeComponentOperation documentTypeComponentOperation,
                                 ListService listService) {
        this.grid = grid;
        this.documentTypeComponentOperation = documentTypeComponentOperation;
        this.listService = listService;
        init();
    }

    void init() {
        setTitle(Transl.get("New document type"));

        CheckboxGroup<EnumerationDto> allowedFormats = new CheckboxGroup<>(Transl.get("Allowed formats"));

        DocumentTypeDetailTabComponent documentTypeDetailTabComponent =
                new DocumentTypeDetailTabComponent(this, true,
                        listService.getEnumerationDtoList("DOCUMENT_FORMAT"), allowedFormats);
        setContent(documentTypeDetailTabComponent);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            if (binder.validate().isOk()) {
                documentTypeComponentOperation.getSaveAction(this, allowedFormats).saveItem(dto, null);
                if (grid != null) {
                    grid.loadData();
                }
            } else {
                ErrorNotification.show(
                        Transl.get(TextValues.INVALIS_INPUT), documentTypeComponentOperation.getAppEnv());
            }
            submit.setEnabled(true);
        });

        showWarning(true);

        addCloseButton();
        if (SecurityUtils.hasPermission(Permission.DOCUMENT_TYPE_EDIT)) {
            addButtons(submit);
        }
    }

    @Override
    public Binder<DocumentTypeDto> getBinder() {
        return binder;
    }

    @Override
    public DocumentTypeDto getDto() {
        return dto;
    }
}
