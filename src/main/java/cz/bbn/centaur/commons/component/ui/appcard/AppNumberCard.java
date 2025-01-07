package cz.bbn.cerberus.commons.component.ui.appcard;

import com.vaadin.flow.component.html.Label;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppCardActionComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.CountActionInt;
import cz.bbn.cerberus.commons.component.ui.interfaces.RouteAction;

public class AppNumberCard extends AppDashboardCard implements AppCardActionComponent {

    private final CountActionInt countActionInt;

    public AppNumberCard(String title, CountActionInt countActionInt, String route) {
        super(title, route);
        this.setHeight("12em");
        this.setWidth("15em");
        this.countActionInt = countActionInt;
        reloadData();
    }

    public AppNumberCard(String title, CountActionInt countActionInt, RouteAction routeAction) {
        super(title, routeAction);
        this.setHeight("12em");
        this.setWidth("15em");
        this.countActionInt = countActionInt;
        reloadData();
    }

    public AppNumberCard(String title, CountActionInt countActionInt, String route, String width) {
        super(title, route);
        this.setHeight("12em");
        this.setWidth(width);
        this.countActionInt = countActionInt;
        reloadData();
    }

    public void reloadData() {
        getContent().removeAll();
        Label count = new Label(String.valueOf(countActionInt.getCount()));
        count.addClassName("count-number");
        getContent().add(count);
    }
}
