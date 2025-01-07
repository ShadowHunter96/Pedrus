package cz.bbn.cerberus.administration.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.document.DocumentService;
import cz.bbn.cerberus.documenttype.DocumentTypeComponentOperation;
import cz.bbn.cerberus.documenttype.DocumentTypeService;
import cz.bbn.cerberus.documenttype.dto.DocumentTypeDto;
import cz.bbn.cerberus.documenttype.ui.component.DocumentTypeGridComponent;
import cz.bbn.cerberus.documenttype.ui.component.DocumentTypeNewDialog;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class DocumentTypeTabComponent extends TabSimpleComponent {

    public static final int TAB_INDEX = 2;

    private final AppEnv appEnv;
    private final DocumentTypeService documentTypeService;
    private final DocumentService documentService;
    private final DocumentTypeComponentOperation documentTypeComponentOperation;
    private final ListService listService;

    private DocumentTypeGridComponent documentTypeGridComponent;

    public DocumentTypeTabComponent(AppEnv appEnv, DocumentTypeService documentTypeService,
                                    DocumentService documentService,
                                    DocumentTypeComponentOperation documentTypeComponentOperation,
                                    ListService listService) {
        this.appEnv = appEnv;
        this.documentTypeService = documentTypeService;
        this.documentService = documentService;
        this.documentTypeComponentOperation = documentTypeComponentOperation;
        this.listService = listService;
        initComponent();
    }

    public void initComponent() {
        setSizeFull();
        setId(RobotFrameworkVariables.DOCUMENT_TYPE_TAB_CARD_ID.getValue());

        documentTypeGridComponent =
                new DocumentTypeGridComponent(getDeleteAction(), appEnv, getItemsAction(),
                        getListAction(), getListActionForChange(), getSaveActionChange());
        documentTypeGridComponent.loadData();
        add(documentTypeGridComponent);

    }

    @Override
    public Button getFooterButton() {
        if (SecurityUtils.hasPermission(Permission.DOCUMENT_TYPE_EDIT)) {
            Button addNew = VaadinComponents.getNewButton(Transl.get("Add document type"));
            addNew.addClickListener(e ->
                    new DocumentTypeNewDialog(documentTypeGridComponent, documentTypeComponentOperation, listService)
                            .open());
            return addNew;
        }
        return null;
    }

    private ListAction<String> getListAction() {
        return documentService::getUsedContactPersonType;
    }

    private ListAction<DocumentTypeDto> getListActionForChange() {
        return documentTypeService::findAllAllowed;
    }

    private ItemsAction<DocumentTypeDto> getItemsAction() {
        return (query, orderList) ->
                documentTypeService.findDocumentTypeDtoPage(query.getPage(), query.getPageSize(), orderList);
    }

    private SaveAction<String> getSaveActionChange() {
        return (newValue, oldValue) -> {
            try {
                documentService.changeDocumentType(newValue, oldValue);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }


    private DeleteAction getDeleteAction() {
        return id -> {
            try {
                documentTypeService.deleteDocumentType(id);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }
}
