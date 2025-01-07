package cz.bbn.cerberus.label.ui;


import com.vaadin.flow.component.orderedlayout.VerticalLayout;
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
import cz.bbn.cerberus.label.LabelComponentOperation;
import cz.bbn.cerberus.label.LabelService;
import cz.bbn.cerberus.label.dto.LabelDto;
import cz.bbn.cerberus.label.ui.component.LabelDetailCardComponent;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import lombok.extern.slf4j.Slf4j;

@Route(value = LabelDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.LABEL_VIEW)
@Slf4j
public class LabelDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "label-list";

    private final LabelService labelService;
    private final LabelComponentOperation labelComponentOperation;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public LabelDetailView(LabelService labelService, LabelComponentOperation labelComponentOperation, AppEnv appEnv,
                           EntityNewComponentOperation entityNewComponentOperation) {
        this.labelService = labelService;
        this.labelComponentOperation = labelComponentOperation;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(LabelDto dto) {
        VerticalLayout componentLayout = new VerticalLayout();
        LabelDetailCardComponent labelDetailCardComponent = new LabelDetailCardComponent(
                dto, labelComponentOperation.getSaveAction(null, componentLayout),
                SecurityUtils.hasPermission(Permission.LABEL_VIEW), appEnv,
                componentLayout, entityNewComponentOperation);
        this.add(labelDetailCardComponent);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, String param) {
        try {
            LabelDto dto = labelService.getLabelDto(param);
            refreshBreadcrumbText(dto.getId());
            initView(dto);
        } catch (SystemException ex) {
            log.error(TextValues.SYSTEM_EXCEPTION, ex);
            ErrorNotification.show(ex, appEnv);
        }
    }
}
