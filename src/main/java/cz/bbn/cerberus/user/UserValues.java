package cz.bbn.cerberus.user;

import cz.bbn.cerberus.commons.security.AppUser;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.translation.enums.ApplicationTranslation;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.SessionScope;

import javax.annotation.PreDestroy;

@Component
@SessionScope
public class UserValues {

    private ApplicationTranslation applicationTranslation =
            ApplicationTranslation.valueOf(Transl.DEFAULT_LANG.toUpperCase());

    private AppUser appUser;

    private boolean turnOffTranslations = false;

    public void setApplicationTranslation(ApplicationTranslation applicationTranslation) {
        this.applicationTranslation = applicationTranslation;
    }

    @PreDestroy
    public void destroy(){
        SecurityContextHolder.clearContext();
    }

    public ApplicationTranslation getApplicationTranslation() {
        return applicationTranslation;
    }

    public void setTurnOffTranslations(boolean turnOffTranslations) {
        this.turnOffTranslations = turnOffTranslations;
    }

    public boolean isTurnOffTranslations() {
        return turnOffTranslations;
    }

    public void setAppUser(AppUser appUser) {
        this.appUser = appUser;
    }

    public AppUser getAppUser() {
        return appUser;
    }
}
