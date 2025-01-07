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
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.technology.TechnologyComponentOperation;
import cz.bbn.cerberus.technology.TechnologyService;
import cz.bbn.cerberus.technology.ui.component.TechnologyFilterComponent;
import cz.bbn.cerberus.technology.ui.component.TechnologyGridComponent;
import cz.bbn.cerberus.technology.ui.component.TechnologyNewDialog;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class TechnologyTabComponent extends TabSimpleComponent {

    public static final int TAB_INDEX = 8;

    private final TechnologyService technologyService;
    private final TechnologyComponentOperation technologyComponentOperation;
    private final AppEnv appEnv;

    private TechnologyGridComponent technologyGridComponent;

    public TechnologyTabComponent(AppEnv appEnv, TechnologyService technologyService,
                                  TechnologyComponentOperation technologyComponentOperation) {
        this.technologyService = technologyService;
        this.technologyComponentOperation = technologyComponentOperation;
        this.appEnv = appEnv;
        initComponent();
    }

    private void initComponent() {
        setSizeFull();
        setId(RobotFrameworkVariables.TECHNOLOGY_TAB_CARD_ID.getValue());

        Button search = VaadinComponents.getSearchButton();
        TechnologyFilterComponent technologyFilterComponent = new TechnologyFilterComponent(search);
        this.add(technologyFilterComponent);

        technologyGridComponent = new TechnologyGridComponent(getDeleteAction(), appEnv,
                technologyComponentOperation.getItemsAction(technologyFilterComponent));

        technologyGridComponent.loadData();
        search.addClickListener(buttonClickEvent -> technologyGridComponent.loadData());
        this.add(technologyGridComponent);

    }

    @Override
    public Button getFooterButton() {
        if (SecurityUtils.hasPermission(Permission.TECHNOLOGY_EDIT)) {
            Button addNew = VaadinComponents.getNewButton(Transl.get("Add technology"));
            addNew.addClickListener(e -> new TechnologyNewDialog(
                    technologyGridComponent, technologyComponentOperation, appEnv).open());
            return addNew;
        }
        return null;
    }

    private DeleteAction getDeleteAction() {
        return id -> {
            try {
                technologyService.delete(id);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }
}
