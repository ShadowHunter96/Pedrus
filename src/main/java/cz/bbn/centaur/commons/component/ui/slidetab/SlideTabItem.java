package cz.bbn.cerberus.commons.component.ui.slidetab;

import com.vaadin.flow.component.Component;
import lombok.Getter;

@Getter
public class SlideTabItem {

    private final String name;
    private final String text;
    private final Component component;
    private final CountIntIndicator indicator;

    private boolean visible = false;

    public SlideTabItem(String name, String text, Component component) {
        this.name = name;
        this.text = text;
        this.component = component;
        this.indicator = null;
    }

    public SlideTabItem(String name, String text, Component component, CountIntIndicator countIntIndicator) {
        this.name = name;
        this.text = text;
        this.component = component;
        this.indicator = countIntIndicator;
    }

    public void setVisible(boolean visible) {
        this.visible = visible;
    }
}
