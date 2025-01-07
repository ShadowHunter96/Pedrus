package cz.bbn.cerberus.enumeration.ui;

import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.enumeration.EnumerationComponentOperation;
import cz.bbn.cerberus.enumeration.EnumerationService;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.enumeration.ui.component.EnumerationDetailComponent;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import lombok.extern.slf4j.Slf4j;

@Route(value = EnumerationDetailView.ROUTE, layout = MainLayout.class)
@Slf4j
public class EnumerationDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "enumeration-detail";

    private final EnumerationService enumerationService;
    private final EnumerationComponentOperation enumerationComponentOperation;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    private int tabIndex;

    public EnumerationDetailView(EnumerationService enumerationService,
                                 EnumerationComponentOperation enumerationComponentOperation, AppEnv appEnv,
                                 EntityNewComponentOperation entityNewComponentOperation) {
        this.enumerationService = enumerationService;
        this.enumerationComponentOperation = enumerationComponentOperation;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(EnumerationDto enumerationDto) {
        EnumerationDetailComponent dphDetailComponent =
                new EnumerationDetailComponent(enumerationDto, true,
                        appEnv, tabIndex, enumerationComponentOperation,
                        entityNewComponentOperation);
        this.add(dphDetailComponent);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, String param) {
        String[] params = param.split("-");
        tabIndex = Integer.parseInt(params[1]);
        try {
            EnumerationDto dto = enumerationService.getEnumerationDto(Long.valueOf(params[0]));
            refreshBreadcrumbText(dto.getName());
            initView(dto);
        } catch (SystemException ex) {
            log.error(TextValues.SYSTEM_EXCEPTION, ex);
            ErrorNotification.show(ex, appEnv);
        }
    }
}
