package cz.bbn.cerberus.administration.ui.component;

import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;

import java.util.List;

public class AdministrationTabsComponent extends TabsComponent<TabSimpleComponent> {
    public AdministrationTabsComponent(String title, List<TabEntry> list, int activeTab,
                                       EntityNewComponentOperation entityNewComponentOperation) {
        super(title, list, activeTab, null, entityNewComponentOperation);
        getContent().setMargin(false);
    }
}
