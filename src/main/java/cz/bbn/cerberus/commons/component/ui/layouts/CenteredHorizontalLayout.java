package cz.bbn.cerberus.commons.component.ui.layouts;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;

public class CenteredHorizontalLayout extends HorizontalLayout {

    public CenteredHorizontalLayout(Component... components) {
        setMargin(false);
        setPadding(false);
        setAlignItems(Alignment.CENTER);
        setJustifyContentMode(JustifyContentMode.CENTER);
        setVerticalComponentAlignment(Alignment.CENTER);
        setWidthFull();
        add(components);
    }
}
