package cz.bbn.cerberus.project.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.component.ui.tab.TabsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.dto.ProjectDto;

import java.util.List;

public class ProjectTabsComponent extends TabsComponent<ProjectDto> {

    public ProjectTabsComponent(String title, List<TabEntry> tabEntryList, Button linkContact, Button linkSupplier,
                                boolean hasCustomPermission, Button addDocument, Button linkActivity,
                                Button linkEmployee, Button addPhase,
                                EntityNewComponentOperation entityNewComponentOperation, ProjectDto dto) {
        super(title, tabEntryList, entityNewComponentOperation);
        if (SecurityUtils.hasPermission(Permission.CONTRACT_EDIT) && hasCustomPermission) {
            if (SecurityUtils.hasPermission(Permission.DOCUMENT_EDIT)) {
                this.addToFooter(addDocument);
            }
            if (SecurityUtils.hasCustomPermission(DomainEnum.PROJECT_DOMAIN_NAME.getValue(), dto.getId(),
                    Permission.PROJECT_LINK_CONTACT_PERSON.name())) {
                this.addToFooter(linkContact);
            }
            if (SecurityUtils.hasPermission(Permission.SUPPLIER_EDIT)) {
                this.addToFooter(linkSupplier);
            }
            if (SecurityUtils.hasPermission(Permission.ACTIVITY_BY_OBJECT_LINK)) {
                this.addToFooter(linkActivity);
            }
            if (SecurityUtils.hasPermission(Permission.EMPLOYEE_BY_OBJECT_LINK)) {
                this.addToFooter(linkEmployee);
            }
            if (SecurityUtils.hasPermission(Permission.PHASE_EDIT)) {
                this.addToFooter(addPhase);
            }
            this.addBackButton();
            this.addSaveButton();
        } else {
            this.addBackButton();
        }
    }
}
