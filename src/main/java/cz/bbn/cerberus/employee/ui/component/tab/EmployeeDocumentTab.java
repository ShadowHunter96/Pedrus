package cz.bbn.cerberus.employee.ui.component.tab;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.document.DocumentComponentOperation;
import cz.bbn.cerberus.document.DocumentObjectEnum;
import cz.bbn.cerberus.document.ui.component.DocumentComponent;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.permission.Permission;

public class EmployeeDocumentTab extends TabSimpleComponent {

    private final DocumentComponentOperation documentComponentOperation;
    private final EmployeeDto dto;
    private final boolean hasCustomPermission;
    private final AppEnv appEnv;
    private final Button addDocument;
    private final ListService listService;

    private DocumentComponent documentComponent;

    public EmployeeDocumentTab(DocumentComponentOperation documentComponentOperation, EmployeeDto dto,
                               boolean hasCustomPermission, AppEnv appEnv, Button addDocument,
                               ListService listService) {
        this.documentComponentOperation = documentComponentOperation;
        this.dto = dto;
        this.hasCustomPermission = hasCustomPermission;
        this.appEnv = appEnv;
        this.addDocument = addDocument;
        this.listService = listService;
        initTab();
    }

    private void initTab() {
        removeAll();
        documentComponent = new DocumentComponent(
                documentComponentOperation, dto.getId(), appEnv,
                DocumentObjectEnum.EMPLOYEE, Permission.EMPLOYEE_DOCUMENT_UPLOAD, Permission.EMPLOYEE_DOCUMENT_DOWNLOAD,
                Permission.EMPLOYEE_DOCUMENT_DELETE, hasCustomPermission, addDocument, listService);
        this.setSizeFull();
        this.add(documentComponent);
    }

    @Override
    public void loadTab() {
        documentComponent.getGrid().loadData();
    }
}
