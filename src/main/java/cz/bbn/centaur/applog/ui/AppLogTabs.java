package cz.bbn.cerberus.applog.ui;

import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.mainlayout.ui.Navigation;

import java.util.List;

public class AppLogTabs extends TabsComponent<TabSimpleComponent> {

    public AppLogTabs(String title, List<TabEntry> tabEntryList, Integer activeTab, Navigation navigation,
                      EntityNewComponentOperation entityNewComponentOperation) {
        super(title, tabEntryList, activeTab, navigation, entityNewComponentOperation);
        this.setSizeFull();
    }
}
