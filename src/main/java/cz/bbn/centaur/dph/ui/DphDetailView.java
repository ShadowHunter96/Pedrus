package cz.bbn.cerberus.dph.ui;

import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.dph.DphComponentOperation;
import cz.bbn.cerberus.dph.DphService;
import cz.bbn.cerberus.dph.dto.DphDto;
import cz.bbn.cerberus.dph.ui.component.DphDetailComponent;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import lombok.extern.slf4j.Slf4j;

@Route(value = DphDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.DPH_VIEW)
@Slf4j
public class DphDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "dph-detail";

    private final DphService dphService;
    private final DphComponentOperation dphComponentOperation;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public DphDetailView(DphService dphService, DphComponentOperation dphComponentOperation, AppEnv appEnv,
                         EntityNewComponentOperation entityNewComponentOperation) {
        this.dphService = dphService;
        this.dphComponentOperation = dphComponentOperation;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(DphDto dphDto) {
        DphDetailComponent dphDetailComponent =
                new DphDetailComponent(dphDto, dphComponentOperation.getSaveAction(null),
                        SecurityUtils.hasPermission(Permission.DPH_EDIT), appEnv, entityNewComponentOperation);
        this.add(dphDetailComponent);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, String param) {
        try {
            DphDto dto = dphService.getDphDto(param);
            refreshBreadcrumbText(dto.getId());
            initView(dto);
        } catch (SystemException ex) {
            log.error(TextValues.SYSTEM_EXCEPTION, ex);
            ErrorNotification.show(ex, appEnv);
        }
    }
}
