package cz.bbn.cerberus.contactperson.ui.components;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;

import java.util.List;

public class ContactPersonTabsComponent extends TabsComponent<TabSimpleComponent> {

    public ContactPersonTabsComponent(String title, List<TabEntry> tabEntryList, boolean showSubmitButton,
                                      Button addDocument, EntityNewComponentOperation entityNewComponentOperation) {
        super(title, tabEntryList, entityNewComponentOperation);
        if (SecurityUtils.hasOneOfPermissions(Permission.DOCUMENT_EDIT, Permission.CONTACT_PERSON_EDIT)) {
            addToButtonFooter(addDocument);
        }
        addBackButton();
        if (showSubmitButton) {
            addSaveButton();
        }
    }

}
