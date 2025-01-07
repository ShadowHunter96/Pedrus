package cz.bbn.cerberus.exception;

import com.vaadin.flow.server.ServiceException;
import com.vaadin.flow.server.ServiceInitEvent;
import com.vaadin.flow.server.SessionInitEvent;
import com.vaadin.flow.server.SessionInitListener;
import com.vaadin.flow.server.VaadinServiceInitListener;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.exception.handler.AppErrorHandler;
import org.springframework.stereotype.Component;


@Component
public class ExceptionWebServlet implements VaadinServiceInitListener, SessionInitListener {

    private final AppEnv appEnv;

    public ExceptionWebServlet(AppEnv appEnv) {
        this.appEnv = appEnv;
    }

    @Override
    public void sessionInit(SessionInitEvent event) throws ServiceException {
        event.getSession().setErrorHandler(new AppErrorHandler(appEnv));
    }

    @Override
    public void serviceInit(ServiceInitEvent event) {
        event.getSource().addSessionInitListener(this);
    }
}
