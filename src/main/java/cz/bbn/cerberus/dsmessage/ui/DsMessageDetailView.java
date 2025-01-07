package cz.bbn.cerberus.dsmessage.ui;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.interfaces.GetItemAction;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.dsmessage.DsMessageService;
import cz.bbn.cerberus.dsmessage.dto.DsMessageDto;
import cz.bbn.cerberus.dsmessage.ui.component.DsMessageDetailComponent;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import lombok.extern.slf4j.Slf4j;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

@Route(value = DsMessageDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.DS_MESSAGE_VIEW)
@Slf4j
public class DsMessageDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "ds-message-detail";

    private final DsMessageService dsMessageService;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public DsMessageDetailView(DsMessageService dsMessageService, AppEnv appEnv,
                               EntityNewComponentOperation entityNewComponentOperation) {
        this.dsMessageService = dsMessageService;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(DsMessageDto dto) {
        DsMessageDetailComponent dsMessageDetailComponent = new DsMessageDetailComponent(dto, null,
                SecurityUtils.hasCustomPermission(DomainEnum.DS_MESSAGE_DOMAIN_NAME.getValue(),
                        dto.getRecipientId(), Permission.DS_MESSAGE_EDIT.name()),
                appEnv, getItemAction(), entityNewComponentOperation);
        dsMessageDetailComponent.addNewEntitySlideTab(
                new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.BACKOFFICE));
        this.add(dsMessageDetailComponent);
    }

    private GetItemAction<InputStream> getItemAction() {
        return id -> {
            byte[] file = dsMessageService.getAttachementFile(Long.valueOf(id));
            return new ByteArrayInputStream(file);
        };
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String param) {
        if (param != null) {
            try {
                DsMessageDto dto = dsMessageService.getMessageDto(Long.valueOf(param));
                if (SecurityUtils.hasCustomPermission(DomainEnum.DS_MESSAGE_DOMAIN_NAME.getValue(),
                        dto.getRecipientId(), Permission.DS_MESSAGE_VIEW.name())) {
                    dsMessageService.updateViewed(dto.getId());
                    initView(dto);
                } else {
                    ErrorNotification.show(ErrorCode.VIEW_PERMISSION_MISSING.getError(), appEnv);
                    UI.getCurrent().access(
                            () -> UI.getCurrent().getPage().fetchCurrentURL(e -> UI.getCurrent().navigate(e.getPath()))
                    );
                }
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        }
    }
}
