package cz.bbn.cerberus.dashboard.ui.component;

import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;

import java.util.List;

public class DashboardTabsComponent extends TabsComponent<TabSimpleComponent> {

    public DashboardTabsComponent(String title, List<TabEntry> list,
                                  EntityNewComponentOperation entityNewComponentOperation) {
        super(title, list, entityNewComponentOperation);
        showFooter(false);
        setMargin(false);
    }
}
