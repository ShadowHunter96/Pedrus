package cz.bbn.cerberus.exception.handler;

import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.router.BeforeEnterEvent;
import com.vaadin.flow.router.ErrorParameter;
import com.vaadin.flow.router.HasErrorParameter;
import com.vaadin.flow.router.ParentLayout;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.exception.ui.ExceptionComponent;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

import javax.annotation.security.PermitAll;
import javax.servlet.http.HttpServletResponse;
import java.nio.file.AccessDeniedException;

@Slf4j
@ParentLayout(MainLayout.class)
@PermitAll
public class AccessDeniedExceptionHandler
        extends VerticalLayout
        implements HasErrorParameter<AccessDeniedException> {

    public final AppEnv appEnv;

    public AccessDeniedExceptionHandler(AppEnv appEnv) {
        this.appEnv = appEnv;
    }

    @Override
    public int setErrorParameter(BeforeEnterEvent event, ErrorParameter<AccessDeniedException> parameter) {

        this.removeAll();

        this.setHeightFull();
        this.setAlignItems(Alignment.CENTER);
        this.setJustifyContentMode(JustifyContentMode.CENTER);

        ExceptionComponent exceptionComponent = new ExceptionComponent(Transl.get("Access denied!"),
                Transl.get("You dont have sufficient permissions to access/perform this content/action."), appEnv);
        this.add(exceptionComponent);
        log.error(TextValues.UNHANDLED_ERROR, parameter.getException());

        return HttpServletResponse.SC_FORBIDDEN;
    }

}
