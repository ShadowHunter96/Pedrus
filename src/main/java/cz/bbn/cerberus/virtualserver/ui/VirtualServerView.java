package cz.bbn.cerberus.virtualserver.ui;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.virtualserver.VirtualServerComponentOperation;
import cz.bbn.cerberus.virtualserver.ui.component.VirtualServerFilterComponent;
import cz.bbn.cerberus.virtualserver.ui.component.VirtualServerGridComponent;

@Route(value = VirtualServerView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.VIRTUAL_SERVER_VIEW)
public class VirtualServerView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "virtual-server-view";

    private final VirtualServerComponentOperation componentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final AppEnv appEnv;

    public VirtualServerView(VirtualServerComponentOperation componentOperation,
                             EntityNewComponentOperation entityNewComponentOperation, AppEnv appEnv) {
        this.componentOperation = componentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.appEnv = appEnv;
    }

    private void initView(String params) {
        removeAll();

        boolean isInfrastructure = componentOperation.getIsInfrastructure();

        Button search = VaadinComponents.getSearchButton();
        VirtualServerFilterComponent virtualServerFilterComponent = new VirtualServerFilterComponent(search,
                componentOperation.findUserList(), params, getHistoryBreadcrumbs(), isInfrastructure);

        VirtualServerGridComponent grid = new VirtualServerGridComponent(componentOperation.getDeleteAction(), appEnv,
                componentOperation.getItemsAction(virtualServerFilterComponent), componentOperation);

        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Virtual server list"),
                Permission.VIRTUAL_SERVER_VIEW, Transl.get("Add virtual server"),
                componentOperation.getNewVirtualServerEvent(grid),
                entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY
        );

        card.setId(RobotFrameworkVariables.VIRTUAL_SERVER_VIEW_CARD_ID.getValue());
        card.add(virtualServerFilterComponent);
        card.add(grid);

        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation));
        add(card);
        grid.loadData();
        search.addClickListener(buttonClickEvent -> {
            grid.loadData();
            virtualServerFilterComponent.fillUrl();
        });
    }

    @Override
    public void setParameter(BeforeEvent event, @OptionalParameter String parameter) {
        initView(parameter);
    }
}
