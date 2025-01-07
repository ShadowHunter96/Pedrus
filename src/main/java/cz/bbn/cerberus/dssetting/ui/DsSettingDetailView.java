package cz.bbn.cerberus.dssetting.ui;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.dssetting.DsSettingComponentOperation;
import cz.bbn.cerberus.dssetting.DsSettingService;
import cz.bbn.cerberus.dssetting.dto.DsSettingDto;
import cz.bbn.cerberus.dssetting.ui.components.DsSettingDetailComponent;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import lombok.extern.slf4j.Slf4j;

@Route(value = DsSettingDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.DS_SETTINGS_VIEW)
@Slf4j
public class DsSettingDetailView extends AppView implements HasUrlParameter<String> {


    public static final String ROUTE = "ds-setting-detail";

    private final DsSettingService dsSettingService;
    private final DsSettingComponentOperation dsSettingComponentOperation;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public DsSettingDetailView(DsSettingService dsSettingService,
                               DsSettingComponentOperation dsSettingComponentOperation,
                               AppEnv appEnv, EntityNewComponentOperation entityNewComponentOperation) {
        this.dsSettingService = dsSettingService;
        this.dsSettingComponentOperation = dsSettingComponentOperation;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(DsSettingDto dto) {
        DsSettingDetailComponent dsSettingDetailComponent = new DsSettingDetailComponent(dto,
                dsSettingComponentOperation.getSaveAction(null),
                SecurityUtils.hasCustomPermission(DomainEnum.DS_SETTING_DOMAIN_NAME.getValue(), dto.getId(),
                        Permission.DS_SETTINGS_EDIT.name()),
                appEnv, dsSettingComponentOperation.getUserList(), entityNewComponentOperation);
        this.add(dsSettingDetailComponent);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String param) {
        DsSettingDto dto = new DsSettingDto();
        dto.setDeleted(Boolean.FALSE);
        dto.setPassword("");
        dto.setLogin("");
        if (param != null) {
            if (SecurityUtils.hasCustomPermission(DomainEnum.DS_SETTING_DOMAIN_NAME.getValue(), param,
                    Permission.DS_SETTINGS_VIEW.name())) {
                try {
                    dto = dsSettingService.getDsSettingDto(param);
                    refreshBreadcrumbText(dto.getId());
                    initView(dto);
                } catch (SystemException ex) {
                    log.error(TextValues.SYSTEM_EXCEPTION, ex);
                    ErrorNotification.show(ex, appEnv);
                }
            }
        } else {
            ErrorNotification.show(ErrorCode.VIEW_PERMISSION_MISSING.getError(), appEnv);
            UI.getCurrent().access(() -> UI.getCurrent().navigate(AdministrationView.ROUTE));
        }
    }
}
