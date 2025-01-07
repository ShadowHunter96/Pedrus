package cz.bbn.cerberus.contract.ui.component.tab;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.interfaces.GridReloadOperation;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.document.DocumentComponentOperation;
import cz.bbn.cerberus.document.DocumentObjectEnum;
import cz.bbn.cerberus.document.ui.component.DocumentComponent;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.permission.Permission;

public class ContractDocumentTab extends TabSimpleComponent {

    private final DocumentComponentOperation documentComponentOperation;
    private final ContractDto dto;
    private final AppEnv appEnv;
    private final GridReloadOperation gridReloadOperation;
    private final boolean hasCustomPermission;
    private final Button addDocument;
    private final ListService listService;

    private DocumentComponent documentComponent;

    public ContractDocumentTab(DocumentComponentOperation documentComponentOperation, ContractDto dto,
                               GridReloadOperation gridReloadOperation, boolean hasCustomPermission, AppEnv appEnv,
                               Button addDocument, ListService listService) {
        this.documentComponentOperation = documentComponentOperation;
        this.dto = dto;
        this.gridReloadOperation = gridReloadOperation;
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
                DocumentObjectEnum.CONTRACT, Permission.CONTRACT_DOCUMENT_UPLOAD, Permission.CONTRACT_DOCUMENT_DOWNLOAD,
                Permission.CONTRACT_DOCUMENT_DELETE, gridReloadOperation, hasCustomPermission, addDocument, listService
        );
        this.setSizeFull();
        this.add(documentComponent);
    }

    @Override
    public void loadTab() {
        documentComponent.getGrid().loadData();
    }
}
