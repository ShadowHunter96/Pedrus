package cz.bbn.cerberus.area.ui;

import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.area.AreaComponentOperation;
import cz.bbn.cerberus.area.AreaService;
import cz.bbn.cerberus.area.dto.AreaDto;
import cz.bbn.cerberus.area.ui.component.AreaDetailComponent;
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
import lombok.extern.slf4j.Slf4j;

@Route(value = AreaDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.AREA_VIEW)
@Slf4j
public class AreaDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "area-detail";

    private final AreaService areaService;
    private final AreaComponentOperation areaComponentOperation;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public AreaDetailView(AreaService areaService, AreaComponentOperation areaComponentOperation, AppEnv appEnv,
                          EntityNewComponentOperation entityNewComponentOperation) {
        this.areaService = areaService;
        this.areaComponentOperation = areaComponentOperation;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(AreaDto areaDto) {
        AreaDetailComponent areaDetailComponent =
                new AreaDetailComponent(areaDto, areaComponentOperation.getSaveAction(null),
                        SecurityUtils.hasPermission(Permission.AREA_EDIT), appEnv, entityNewComponentOperation);
        this.add(areaDetailComponent);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, String param) {
        AreaDto dto = new AreaDto();
        if (param != null) {
            try {
                dto = areaService.getAreaDto(param);
                refreshBreadcrumbText(dto.getId());
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        }
        initView(dto);
    }
}
