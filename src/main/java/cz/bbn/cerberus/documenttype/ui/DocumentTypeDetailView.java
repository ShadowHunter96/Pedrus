package cz.bbn.cerberus.documenttype.ui;

import com.vaadin.flow.component.checkbox.CheckboxGroup;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.documenttype.DocumentTypeComponentOperation;
import cz.bbn.cerberus.documenttype.DocumentTypeService;
import cz.bbn.cerberus.documenttype.dto.DocumentTypeDto;
import cz.bbn.cerberus.documenttype.ui.component.DocumentTypeDetailComponent;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Route(value = DocumentTypeDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.DOCUMENT_VIEW)
@Slf4j
public class DocumentTypeDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "document-type-detail";

    private final DocumentTypeService documentTypeService;
    private final DocumentTypeComponentOperation documentTypeComponentOperation;
    private final AppEnv appEnv;
    private final ListService listService;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public DocumentTypeDetailView(DocumentTypeService documentTypeService,
                                  DocumentTypeComponentOperation documentTypeComponentOperation,
                                  AppEnv appEnv, ListService listService,
                                  EntityNewComponentOperation entityNewComponentOperation) {
        this.documentTypeService = documentTypeService;
        this.documentTypeComponentOperation = documentTypeComponentOperation;
        this.appEnv = appEnv;
        this.listService = listService;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(DocumentTypeDto dto) {
        removeAll();
        setSizeFull();
        CheckboxGroup<EnumerationDto> allowedCheckboxGroup = new CheckboxGroup<>(Transl.get("Allowed formats"));
        DocumentTypeDetailComponent documentTypeDetailComponent =
                new DocumentTypeDetailComponent(dto,
                        documentTypeComponentOperation.getSaveAction(null, allowedCheckboxGroup),
                        SecurityUtils.hasPermission(Permission.DOCUMENT_TYPE_EDIT),
                        appEnv, listService, allowedCheckboxGroup, entityNewComponentOperation);
        this.add(documentTypeDetailComponent);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String param) {
        if (param != null) {
            try {
                DocumentTypeDto dto = documentTypeService.getDocumentType(param);
                refreshBreadcrumbText(dto.getId());
                initView(dto);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        }
    }
}
