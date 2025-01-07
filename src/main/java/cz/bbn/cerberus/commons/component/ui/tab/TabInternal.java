package cz.bbn.cerberus.commons.component.ui.tab;

import com.vaadin.flow.component.tabs.Tab;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TabInternal {

    private Tab tab;
    private TabSimpleComponent tabSimpleComponent;
    private int tabIndex;

    public TabInternal(Tab tab, TabSimpleComponent tabSimpleComponent, int tabIndex) {
        this.tab = tab;
        this.tabSimpleComponent = tabSimpleComponent;
        this.tabIndex = tabIndex;
    }
}
