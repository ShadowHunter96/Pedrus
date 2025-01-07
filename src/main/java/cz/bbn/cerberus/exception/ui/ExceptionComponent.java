package cz.bbn.cerberus.exception.ui;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Text;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.H2;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.server.VaadinServletRequest;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.enviromennt.AppProperty;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.dashboard.ui.DashboardView;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;


@Slf4j
public class ExceptionComponent extends VerticalLayout {

    public final String errorTitle;
    public final String errorMessage;

    public final AppEnv appEnv;

    public ExceptionComponent(String errorTitle, String errorMessage, AppEnv appEnv) {
        this.errorTitle = errorTitle;
        this.errorMessage = errorMessage;
        this.appEnv = appEnv;
        init();
    }

    public void init() {
        this.removeAll();

        this.setHeightFull();
        this.setAlignItems(Alignment.CENTER);
        this.setJustifyContentMode(JustifyContentMode.CENTER);

        Button dashboardButton = VaadinComponents.getButton(Transl.get("Go to dashboard"));
        dashboardButton.addClickListener(buttonClickEvent ->
                UI.getCurrent().navigate(DashboardView.ROUTE)
        );
        Button logoutButton = VaadinComponents.getButton(Transl.get("Logout"));
        logoutButton.addClickListener(buttonClickEvent -> {
                    if (SecurityUtils.userIsAzureInstance()) {
                        UI.getCurrent().getPage().setLocation(appEnv.getStringProperty(AppProperty.AZURE_LOGOUT));
                    }
                    VaadinServletRequest.getCurrent().getHttpServletRequest().getSession().invalidate();
                }
        );

        this.add(this.getIcon());
        this.add(new H2(errorTitle));
        this.add(new Div(new Text(errorMessage)));

        HorizontalLayout buttonLayout = new HorizontalLayout();
        buttonLayout.add(dashboardButton, logoutButton);
        this.add(buttonLayout);
    }

    private Component getIcon() {
        Icon ico = new Icon(VaadinIcon.EXCLAMATION_CIRCLE_O);

        ico.setSize("4em");
        ico.setColor(CssVariables.LUMO_ERROR_COLOR.getValue());

        return ico;
    }

}
