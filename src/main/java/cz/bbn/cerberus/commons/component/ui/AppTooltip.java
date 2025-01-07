package cz.bbn.cerberus.commons.component.ui;

import com.vaadin.componentfactory.Tooltip;
import com.vaadin.componentfactory.TooltipAlignment;
import com.vaadin.componentfactory.TooltipPosition;

public class AppTooltip extends Tooltip {

    public AppTooltip(){
        super();
        setThemeName("light");
        setAlignment(TooltipAlignment.RIGHT);
        setPosition(TooltipPosition.RIGHT);
    }
}
