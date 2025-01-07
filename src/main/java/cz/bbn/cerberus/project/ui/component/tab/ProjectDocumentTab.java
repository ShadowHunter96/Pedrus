package cz.bbn.cerberus.project.ui.component.tab;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.document.DocumentComponentOperation;
import cz.bbn.cerberus.document.DocumentObjectEnum;
import cz.bbn.cerberus.document.ui.component.DocumentComponent;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.dto.ProjectDto;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ProjectDocumentTab extends TabSimpleComponent {

    private final DocumentComponentOperation documentComponentOperation;
    private final ProjectDto dto;
    private final boolean hasCustomPermission;
    private final AppEnv appEnv;
    private final Button addDocument;
    private final ListService listService;

    private DocumentComponent documentComponent;

    public ProjectDocumentTab(DocumentComponentOperation documentComponentOperation,
                              ProjectDto dto, boolean hasCustomPermission, AppEnv appEnv,
                              Button addDocument, ListService listService) {
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
                DocumentObjectEnum.PROJECT, Permission.PROJECT_DOCUMENT_UPLOAD, Permission.PROJECT_DOCUMENT_DOWNLOAD,
                Permission.PROJECT_DOCUMENT_DELETE, hasCustomPermission, addDocument, listService);
        this.setSizeFull();
        this.add(documentComponent);
    }

    @Override
    public void loadTab() {
        documentComponent.getGrid().loadData();
    }
}
