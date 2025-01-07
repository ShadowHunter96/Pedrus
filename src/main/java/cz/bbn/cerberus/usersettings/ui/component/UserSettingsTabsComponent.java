package cz.bbn.cerberus.usersettings.ui.component;

import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;

import java.util.List;

public class UserSettingsTabsComponent extends TabsComponent<TabSimpleComponent> {

    public UserSettingsTabsComponent(String title, List<TabEntry> list, int activeTab,
                                     EntityNewComponentOperation entityNewComponentOperation) {
        super(title, list, activeTab, null, entityNewComponentOperation);
        getContent().setMargin(false);
    }
}