package cz.bbn.cerberus.exception.ui;

import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import org.springframework.context.annotation.Scope;

import javax.annotation.PostConstruct;
import javax.annotation.security.PermitAll;

@Route(value = GlobalExceptionView.ROUTE)
@PermitAll
@Scope(value = "vaadin-ui")
public class GlobalExceptionView extends Div {

    public static final String ROUTE = "global-exception";

    public final AppEnv appEnv;

    public GlobalExceptionView(AppEnv appEnv) {
        this.appEnv = appEnv;
    }

    @PostConstruct
    void init(){
        this.removeAll();
        this.setHeightFull();

        ExceptionComponent exceptionComponent = new ExceptionComponent("Global Internal error!",
                "Unexpected system fail. Please contact your administrator.", appEnv);
        this.add(exceptionComponent);
    }
}
