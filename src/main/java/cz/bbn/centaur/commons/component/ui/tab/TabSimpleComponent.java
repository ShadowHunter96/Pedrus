package cz.bbn.cerberus.commons.component.ui.tab;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import org.apache.commons.lang3.StringUtils;

public abstract class TabSimpleComponent extends VerticalLayout {

    private String name;

    protected TabSimpleComponent() {
        getElement().getStyle().set("overflow", "auto");
    }

    public void saveItem() {
        // Called only when tab has something to save, can be overridden.
    }

    public void loadTab() {
        // Called only when tab has something to load, can be overridden.
    }

    public Button getFooterButton() {
        return null;
    }

    public boolean getClearFooter() {
        return false;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getName() {
        return StringUtils.trimToEmpty(name);
    }
}
