package cz.bbn.cerberus.commons.component.ui.appcard;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.html.H3;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.commons.component.ui.interfaces.RouteAction;
import org.apache.commons.lang3.StringUtils;

public abstract class AppDashboardCard extends VerticalLayout {

    private final VerticalLayout header = new VerticalLayout();
    private final VerticalLayout content = new VerticalLayout();

    private final String route;
    private final RouteAction routeAction;
    private H3 headerText;
    private String title;

    public AppDashboardCard(String title, String route) {
        this.route = route;
        this.routeAction = null;
        this.title = title;
        headerText = new H3(title);
        headerText.addClassName("app-dashboard-card-header-text");
        this.header.add(headerText);
        initAppDashboardNumberComponent();
    }

    public AppDashboardCard(String title, RouteAction routeAction) {
        this.route = null;
        this.routeAction = routeAction;
        this.title = title;
        headerText = new H3(title);
        headerText.addClassName("app-dashboard-card-header-text");
        this.header.add(headerText);
        initAppDashboardNumberComponent();
    }

    private void initAppDashboardNumberComponent() {
        this.addClassName("card-content");
        this.addClassName("dashboard-card-content");

        content.setSizeFull();
        content.setMargin(false);
        content.setPadding(false);
        content.setClassName("app-dashboard-content");

        header.setMargin(false);
        header.setPadding(false);
        header.setClassName("app-dashboard-header");
        header.setHorizontalComponentAlignment(FlexComponent.Alignment.CENTER);
        header.setWidthFull();
        this.addComponentAtIndex(0, header);
        this.addComponentAtIndex(1, content);

        if (!StringUtils.isEmpty(route) || routeAction != null) {
            this.addClickListener(verticalLayoutClickEvent -> {
                if (route != null) {
                    UI.getCurrent().access(() -> UI.getCurrent().navigate(route));
                } else {
                    UI.getCurrent().access(() -> UI.getCurrent().navigate(routeAction.getRoute()));
                }
            });
        }
    }

    public void setHeader(String text) {
        headerText.setText(text);
    }

    public String getTitle() {
        return title;
    }

    public abstract void reloadData();

    public VerticalLayout getContent() {
        return content;
    }
}
