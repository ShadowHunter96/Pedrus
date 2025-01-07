package cz.bbn.cerberus.commons.component.ui.appcard;

import com.vaadin.flow.component.html.Label;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppCardActionComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.StringAction;

public class AppStringCard extends AppDashboardCard implements AppCardActionComponent {

    private final StringAction stringAction;

    public AppStringCard(String title, StringAction stringAction, String route, String width) {
        super(title, route);
        this.setHeight("12em");
        this.setWidth(width);
        this.stringAction = stringAction;
        reloadData();
    }

    public void reloadData() {
        getContent().removeAll();
        Label count = new Label(String.valueOf(stringAction.getValue()));
        count.addClassName("count-number");
        getContent().add(count);
    }
}
