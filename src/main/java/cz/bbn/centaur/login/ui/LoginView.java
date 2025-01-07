package cz.bbn.cerberus.login.ui;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dependency.CssImport;
import com.vaadin.flow.component.dependency.JsModule;
import com.vaadin.flow.component.login.LoginI18n;
import com.vaadin.flow.component.login.LoginOverlay;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.router.BeforeEnterEvent;
import com.vaadin.flow.router.BeforeEnterObserver;
import com.vaadin.flow.router.PageTitle;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.enviromennt.AppProperty;
import org.springframework.context.annotation.Scope;

import javax.servlet.http.HttpServletRequest;


@Route("login")
@PageTitle("Login | PP2")
@CssImport("./styles/custom-styles.css")
@JsModule("./styles/shared-styles.js")
@Scope(value = "vaadin-ui")
public class LoginView extends VerticalLayout implements BeforeEnterObserver {

    private final AppEnv appEnv;

    private final LoginOverlay loginOverlay = new LoginOverlay();
    private final HttpServletRequest request;

    public LoginView(AppEnv appEnv, HttpServletRequest request) {
        this.appEnv = appEnv;
        this.request = request;
        initView();
    }

    public void initView() {

        if (UI.getCurrent() != null) {
            String primColor = appEnv.getStringProperty(AppProperty.ENVIRONMENT_COLOR, "78deg");
            UI.getCurrent().getElement().getStyle().set("--lumo-primary-color-h", primColor + " !important");
        }

        loginOverlay.addClassName("login-rich-content");
        loginOverlay.setTitle("CRB");
        loginOverlay.setDescription("Cerberus");
        loginOverlay.setAction("login");
        loginOverlay.addForgotPasswordListener(e ->
                Notification.show("Forgot password not yet handled", 30,
                        Notification.Position.TOP_CENTER));
        loginOverlay.setOpened(true);
        add(loginOverlay);
    }

    @Override
    public void beforeEnter(BeforeEnterEvent beforeEnterEvent) {
        if (beforeEnterEvent.getLocation()
                .getQueryParameters()
                .getParameters()
                .containsKey("error")) {
            String message = (String) request.getSession().getAttribute("message");
            if (message == null || message.isEmpty()) {
                loginOverlay.setError(true);
            } else {
                setError(message);
            }
        }
        request.getSession().setAttribute("message", "");
    }

    private void setError(String msg) {
        LoginI18n i18n = LoginI18n.createDefault();
        LoginI18n.ErrorMessage em = new LoginI18n.ErrorMessage();
        em.setMessage(msg);
        i18n.setErrorMessage(em);
        loginOverlay.setI18n(i18n);
        loginOverlay.setError(true);
    }
}
