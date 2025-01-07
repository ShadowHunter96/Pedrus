package cz.bbn.cerberus.administration.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.dph.DphComponentOperation;
import cz.bbn.cerberus.dph.DphService;
import cz.bbn.cerberus.dph.ui.component.DphFilterComponent;
import cz.bbn.cerberus.dph.ui.component.DphGridComponent;
import cz.bbn.cerberus.dph.ui.component.DphNewDialog;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class DphTabComponent extends TabSimpleComponent {

    public static final int TAB_INDEX = 9;

    private final DphService dphService;
    private final DphComponentOperation dphComponentOperation;
    private final AppEnv appEnv;

    private DphGridComponent dphGridComponent;

    public DphTabComponent(AppEnv appEnv, DphService dphService, DphComponentOperation dphComponentOperation) {
        this.dphService = dphService;
        this.dphComponentOperation = dphComponentOperation;
        this.appEnv = appEnv;
        initComponent();
    }

    private void initComponent() {
        setSizeFull();
        setId(RobotFrameworkVariables.AREA_TAB_CARD_ID.getValue());

        Button search = VaadinComponents.getSearchButton();
        DphFilterComponent dphFilterComponent = new DphFilterComponent(search);
        this.add(dphFilterComponent);

        dphGridComponent =
                new DphGridComponent(getDeleteAction(), appEnv,
                        dphComponentOperation.getItemsAction(dphFilterComponent), dphComponentOperation);

        dphGridComponent.loadData();
        search.addClickListener(buttonClickEvent -> dphGridComponent.loadData());
        this.add(dphGridComponent);

    }

    @Override
    public Button getFooterButton() {
        if (SecurityUtils.hasPermission(Permission.DPH_EDIT)) {
            Button addNew = VaadinComponents.getNewButton(Transl.get("Add dph"));
            addNew.addClickListener(e -> new DphNewDialog(
                    dphGridComponent, dphComponentOperation, appEnv).open());
            return addNew;
        }
        return null;
    }

    private DeleteAction getDeleteAction() {
        return id -> {
            try {
                dphService.delete(id);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }
}
