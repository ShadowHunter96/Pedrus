package cz.bbn.cerberus.commons.security;

import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.SessionScope;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

@Component
@SessionScope
public class AppSessionListener {

    private LocalDateTime time;
    private final int sessionTimeout;

    public AppSessionListener(AppEnv appEnv) {
        this.sessionTimeout = appEnv.getTimeout();
    }

    public boolean checkSession() {

        if (time == null) {
            time = LocalDateTime.now();
            return false;
        }

        if (ChronoUnit.MINUTES.between(time, LocalDateTime.now()) >= sessionTimeout) {
            time = LocalDateTime.now();
            return true;
        }
        time = LocalDateTime.now();
        return false;
    }
}
