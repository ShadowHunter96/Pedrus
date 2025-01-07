package cz.bbn.cerberus.config;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.router.BeforeEnterEvent;
import com.vaadin.flow.server.ServiceInitEvent;
import com.vaadin.flow.server.UIInitEvent;
import com.vaadin.flow.server.UIInitListener;
import com.vaadin.flow.server.VaadinServiceInitListener;
import com.vaadin.flow.server.VaadinServletRequest;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.security.AppSessionListener;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.commons.security.ui.NotAllowedView;
import cz.bbn.cerberus.mainlayout.ui.Navigation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Vaadin event before route entered
 * checks annotation and delegate autorization to spring security
 */
@Slf4j
@Component
public class ConfigureUIServiceInitListener implements VaadinServiceInitListener, UIInitListener {

    private final AppSessionListener appSessionListener;
    private final Navigation navigation;
    private final HistoryBreadcrumbs historyBreadcrumbs;

    public ConfigureUIServiceInitListener(AppSessionListener appSessionListener, Navigation navigation,
                                          HistoryBreadcrumbs historyBreadcrumbs) {
        this.appSessionListener = appSessionListener;
        this.navigation = navigation;
        this.historyBreadcrumbs = historyBreadcrumbs;
    }

    /**
     * In serviceInit, listen for the initialization of the UI (the internal root component in Vaadin)
     * and then add a listener before every view transition.
     *
     * @param event ServiceInitEvent
     */
    @Override
    public void serviceInit(ServiceInitEvent event) {
        event.getSource().addUIInitListener((UIInitEvent uiEvent) -> {
            final UI ui = uiEvent.getUI();
            ui.addBeforeEnterListener(this::beforeEnter);
        });
        event.getSource().addUIInitListener(this);
    }

    /**
     * In authenticateNavigation, reroute all requests to the login, if the user is not logged in
     *
     * @param event BeforeEnterEvent
     */
    private void beforeEnter(BeforeEnterEvent event) {
        if (appSessionListener.checkSession()) {
            VaadinServletRequest.getCurrent().getHttpServletRequest().getSession().invalidate();
        }
        if (!SecurityUtils.isAccessGranted(event.getNavigationTarget())) {
            event.rerouteTo(NotAllowedView.ROUTE);
        }
        navigation.checkUrlAndChangeMenuItem(event);
        historyBreadcrumbs.reload(event);
    }

    @Override
    public void uiInit(UIInitEvent uiInitEvent) {
        // not implemented yet
    }
}