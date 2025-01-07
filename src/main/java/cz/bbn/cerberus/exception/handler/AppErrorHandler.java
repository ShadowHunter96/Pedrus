package cz.bbn.cerberus.exception.handler;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.server.ErrorEvent;
import com.vaadin.flow.server.ErrorHandler;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class AppErrorHandler implements ErrorHandler {

    private final AppEnv appEnv;

    public AppErrorHandler(AppEnv appEnv) {
        this.appEnv = appEnv;
    }

    @Override
    public void error(ErrorEvent errorEvent) {
        log.error("Error occured", errorEvent.getThrowable());
        if (UI.getCurrent() != null) {
            UI.getCurrent().access(() -> ErrorNotification.show(Transl.get(
                    "An internal error has occurred. Please contact support."), appEnv));
        }
    }
}
