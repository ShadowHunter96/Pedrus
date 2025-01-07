package cz.bbn.cerberus.employee.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;

import java.util.List;

public class EmployeeTabsComponent extends TabsComponent<TabSimpleComponent> {

    public EmployeeTabsComponent(String title, List<TabEntry> tabEntryList,
                                 boolean hasEmployeeEdit, Button addDocument,
                                 EntityNewComponentOperation entityNewComponentOperation) {
        super(title, tabEntryList, entityNewComponentOperation);

        if (SecurityUtils.hasPermission(Permission.EMPLOYEE_DOCUMENT_VIEW) &&
                SecurityUtils.hasPermission(Permission.EMPLOYEE_EDIT) && hasEmployeeEdit) {
            addToButtonFooter(addDocument);
        }

        addBackButton();
        if (SecurityUtils.hasPermission(Permission.EMPLOYEE_EDIT) && hasEmployeeEdit) {
            addSaveButton();
        }
    }
}
