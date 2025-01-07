package cz.bbn.cerberus.commons.security.ui;

import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.router.PageTitle;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCard;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;

import javax.annotation.PostConstruct;

@Route(value = NotAllowedView.ROUTE, layout = MainLayout.class)
@PageTitle("Not Allowed")
@Tag(NotAllowedView.ROUTE)
public class NotAllowedView extends AppView {

    public static final String ROUTE = "notAllowed-view";

    @PostConstruct
    public void init() {
        this.setWidthFull();

        AppCard disabledC = new AppCard(null);
        Span text = new Span("You are not authorized to view this page.");
        text.getElement().getStyle().set("font-size", "1.5em");
        text.getElement().getStyle().set("color", "grey");
        text.getElement().getClassList().add("margin-normal");
        disabledC.add(text);
        disabledC.setWidthFull();
        this.add(disabledC);
    }
}
