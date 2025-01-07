package cz.bbn.cerberus.invoice.ui.component;

import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.component.ui.tab.TabsLayoutComponent;

import java.util.List;

public class InvoicingTabsComponent extends TabsLayoutComponent {

    public InvoicingTabsComponent(List<TabEntry> list) {
        super("", list);
        setSizeFull();
    }

}
