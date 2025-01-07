package cz.bbn.cerberus.commons.component.ui.tab;

import com.vaadin.flow.component.html.H2;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.tabs.Tab;
import com.vaadin.flow.component.tabs.Tabs;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

public class TabsLayoutComponent extends VerticalLayout {

    private List<TabInternal> tabInnerList;
    private final int activeTab = 0;

    public TabsLayoutComponent(String title, List<TabEntry> tabEntryList) {
        init(title, tabEntryList);
    }

    private void init(String title, List<TabEntry> tabEntryList) {
        setMargin(false);
        setPadding(false);
        if (StringUtils.isNoneEmpty(title)) {
            H2 header = new H2(title);
            add(header);
        }

        setSizeFull();
        Tabs tabs = new Tabs();
        this.tabInnerList = getTabMap(tabs, tabEntryList);
        add(tabs);
        if (tabs.getComponentCount() > 0) {
            tabs.setSelectedIndex(activeTab);
        }

        setUpTabs();
        setUpTabChange(tabs, this.tabInnerList);
    }

    public int getSelectedTab() {
        for (int i = 0; i < tabInnerList.size(); i++) {
            if (tabInnerList.get(i).getTab().isSelected()) {
                return i;
            }
        }
        return 0;
    }

    private List<TabInternal> getTabMap(Tabs tabs, List<TabEntry> tabEntryList) {
        List<TabInternal> tabInternal = new ArrayList<>();

        for (TabEntry item : tabEntryList) {
            if (item.getPermission() == null || SecurityUtils.hasPermission(item.getPermission())) {
                Tab tab = new Tab(item.getTitle());
                tabs.add(tab);
                TabInternal tabIntItem = new TabInternal(tab, item.getTabSimpleComponent(), item.getTabIndex());
                tabInternal.add(tabIntItem);
            }
        }

        return tabInternal;
    }

    private void setUpTabs() {
        for (int i = 0, tabInnerListSize = tabInnerList.size(); i < tabInnerListSize; i++) {
            TabInternal intern = tabInnerList.get(i);
            add(intern.getTabSimpleComponent());

            if (activeTab == i) {
                intern.getTab().setSelected(true);
                intern.getTabSimpleComponent().setVisible(true);
            } else {
                intern.getTabSimpleComponent().setVisible(false);
            }
        }
    }

    private void setUpTabChange(Tabs tabs, List<TabInternal> tabList) {
        tabs.addSelectedChangeListener(e -> {
            for (TabInternal item : tabList) {
                if (item.getTab().equals(e.getSelectedTab())) {
                    item.getTab().setSelected(true);
                    item.getTabSimpleComponent().setVisible(true);
                    item.getTabSimpleComponent().loadTab();
                } else {
                    item.getTabSimpleComponent().setVisible(false);
                }
            }
        });
    }
}
