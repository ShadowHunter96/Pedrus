package cz.bbn.cerberus.administration.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.area.AreaComponentOperation;
import cz.bbn.cerberus.area.AreaService;
import cz.bbn.cerberus.area.ui.component.AreaFilterComponent;
import cz.bbn.cerberus.area.ui.component.AreaGridComponent;
import cz.bbn.cerberus.area.ui.component.AreaNewDialog;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class AreaTabComponent extends TabSimpleComponent {

    public static final int TAB_INDEX = 7;

    private final AreaService areaService;
    private final AreaComponentOperation areaComponentOperation;
    private final AppEnv appEnv;

    private AreaGridComponent areaGridComponent;

    public AreaTabComponent(AppEnv appEnv, AreaService areaService, AreaComponentOperation areaComponentOperation) {
        this.areaService = areaService;
        this.areaComponentOperation = areaComponentOperation;
        this.appEnv = appEnv;
        initComponent();
    }

    private void initComponent() {
        setSizeFull();
        setId(RobotFrameworkVariables.AREA_TAB_CARD_ID.getValue());

        Button search = VaadinComponents.getSearchButton();
        AreaFilterComponent areaFilterComponent = new AreaFilterComponent(search);
        this.add(areaFilterComponent);

        areaGridComponent =
                new AreaGridComponent(getDeleteAction(), appEnv,
                        areaComponentOperation.getItemsAction(areaFilterComponent));

        areaGridComponent.loadData();
        search.addClickListener(buttonClickEvent -> areaGridComponent.loadData());
        this.add(areaGridComponent);

    }

    @Override
    public Button getFooterButton() {
        if (SecurityUtils.hasPermission(Permission.AREA_EDIT)) {
            Button addNew = VaadinComponents.getNewButton(Transl.get("Add area"));
            addNew.addClickListener(e -> new AreaNewDialog(
                    areaGridComponent, areaComponentOperation, appEnv).open());
            return addNew;
        }
        return null;
    }

    private DeleteAction getDeleteAction() {
        return id -> {
            try {
                areaService.delete(id);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }
}
