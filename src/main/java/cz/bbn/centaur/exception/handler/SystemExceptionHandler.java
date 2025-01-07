package cz.bbn.cerberus.exception.handler;

import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.router.BeforeEnterEvent;
import com.vaadin.flow.router.ErrorParameter;
import com.vaadin.flow.router.HasErrorParameter;
import com.vaadin.flow.router.ParentLayout;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.exception.ui.ExceptionComponent;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

import javax.annotation.security.PermitAll;
import javax.servlet.http.HttpServletResponse;

@Slf4j
@ParentLayout(MainLayout.class)
@PermitAll
public class SystemExceptionHandler
        extends VerticalLayout
        implements HasErrorParameter<SystemException> {

    public final AppEnv appEnv;

    public SystemExceptionHandler(AppEnv appEnv) {
        this.appEnv = appEnv;
    }

    @Override
    public int setErrorParameter(BeforeEnterEvent event, ErrorParameter<SystemException> parameter) {

        this.removeAll();

        this.setHeightFull();
        this.setAlignItems(Alignment.CENTER);
        this.setJustifyContentMode(JustifyContentMode.CENTER);

        ExceptionComponent exceptionComponent = new ExceptionComponent(Transl.get("System error!"),
                Transl.get("Unexpected system fail. Please contact your administrator."), appEnv);
        this.add(exceptionComponent);
        log.error(TextValues.UNHANDLED_ERROR, parameter.getException());

        return HttpServletResponse.SC_EXPECTATION_FAILED;
    }

}
