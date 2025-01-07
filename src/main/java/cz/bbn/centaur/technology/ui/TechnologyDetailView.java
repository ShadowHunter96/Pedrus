package cz.bbn.cerberus.technology.ui;

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
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.technology.TechnologyComponentOperation;
import cz.bbn.cerberus.technology.TechnologyService;
import cz.bbn.cerberus.technology.dto.TechnologyDto;
import cz.bbn.cerberus.technology.ui.component.TechnologyDetailComponent;
import lombok.extern.slf4j.Slf4j;

@Route(value = TechnologyDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.TECHNOLOGY_VIEW)
@Slf4j
public class TechnologyDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "technology-detail";

    private final TechnologyService technologyService;
    private final TechnologyComponentOperation technologyComponentOperation;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public TechnologyDetailView(TechnologyService technologyService,
                                TechnologyComponentOperation technologyComponentOperation,
                                AppEnv appEnv, EntityNewComponentOperation entityNewComponentOperation) {
        this.technologyService = technologyService;
        this.technologyComponentOperation = technologyComponentOperation;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(TechnologyDto technologyDto) {
        TechnologyDetailComponent technologyDetailComponent =
                new TechnologyDetailComponent(technologyDto, technologyComponentOperation.getSaveAction(null),
                        SecurityUtils.hasPermission(Permission.TECHNOLOGY_EDIT), appEnv, entityNewComponentOperation);
        this.add(technologyDetailComponent);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, String param) {
        if (param != null) {
            try {
                TechnologyDto dto = technologyService.getTechnologyDto(param);
                refreshBreadcrumbText(dto.getId());
                initView(dto);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        }
    }
}
