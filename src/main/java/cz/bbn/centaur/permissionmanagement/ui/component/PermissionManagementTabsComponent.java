package cz.bbn.cerberus.permissionmanagement.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;

import java.util.List;

public class PermissionManagementTabsComponent extends TabsComponent<TabSimpleComponent> {

    public PermissionManagementTabsComponent(String title, List<TabEntry> tabEntryList, Button saveButton,
                                             EntityNewComponentOperation entityNewComponentOperation) {
        super(title, tabEntryList, entityNewComponentOperation);
        addSaveButton(saveButton);
    }
}
