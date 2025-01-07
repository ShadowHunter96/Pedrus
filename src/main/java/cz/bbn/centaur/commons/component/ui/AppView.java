package cz.bbn.cerberus.commons.component.ui;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.spring.annotation.SpringComponent;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.config.SpringContext;
import org.springframework.context.annotation.Scope;

import javax.annotation.security.PermitAll;

@PermitAll
// tohle je kvuli spring security. Nas projekt filtruje pristup na zaklade opravneni ne role,
// proto tady vsechny role povolujeme, ale hloubeji se resi pristup na zaklade opravneni
@Scope(value = "vaadin-ui")
@SpringComponent
public class AppView extends VerticalLayout {

    private final HistoryBreadcrumbs historyBreadcrumbs;

    public AppView() {
        this.setSizeFull();
        setMinWidth("18.75rem");
        addClassName("project-view-class");
        this.historyBreadcrumbs = SpringContext.getBean(HistoryBreadcrumbs.class);
        this.add(this.historyBreadcrumbs.getBreadcrumbs());
    }

    @Override
    protected void onAttach(AttachEvent event) {
        super.onAttach(event);
        setMinWidth("18.75rem");
    }

    @Override
    public void removeAll() {
        super.removeAll();
        this.add(historyBreadcrumbs.getBreadcrumbs());
    }

    public void refreshBreadcrumbText(String text) {
        historyBreadcrumbs.refreshTextBreadcrumb(text);
    }

    public HistoryBreadcrumbs getHistoryBreadcrumbs() {
        return historyBreadcrumbs;
    }
}
