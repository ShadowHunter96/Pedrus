package cz.bbn.cerberus.offer.ui;

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
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.offer.OfferComponentOpperation;
import cz.bbn.cerberus.offer.ui.component.OfferFilterComponent;
import cz.bbn.cerberus.offer.ui.component.OfferGridComponent;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Route(value = OfferView.ROUTE, layout = MainLayout.class)
@Authorize({Permission.OFFER_VIEW, Permission.OFFER_LIST_VIEW})
@Slf4j
public class OfferView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "offer-list";

    private final ListService listService;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final OfferComponentOpperation offerComponentOpperation;
    private final AppEnv appEnv;
    private OfferFilterComponent offerFilterComponent;
    private OfferGridComponent grid;

    public OfferView(ListService listService, EntityNewComponentOperation entityNewComponentOperation,
                     OfferComponentOpperation offerComponentOpperation, AppEnv appEnv) {
        this.listService = listService;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.offerComponentOpperation = offerComponentOpperation;
        this.appEnv = appEnv;
    }

    private void initView(String params) {
        removeAll();
        setSizeFull();

        Button search = VaadinComponents.getSearchButton();
        offerFilterComponent =
                new OfferFilterComponent(search, listService, params, getHistoryBreadcrumbs());
        OfferGridComponent grid = new OfferGridComponent(offerComponentOpperation.getDeleteAction(), appEnv,
                offerComponentOpperation.getItemsAction(offerFilterComponent), 2);
        offerFilterComponent.setGrid(grid);
        offerFilterComponent.fillFilterFromUrl();

        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Offer list"),
                Permission.OFFER_EDIT, Transl.get("Add offer"),
                entityNewComponentOperation.getNewOfferEvent(grid, null, null),
                entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY
        );
        card.setId(RobotFrameworkVariables.OPPORTUNITY_VIEW_CARD_ID.getValue());
        card.add(offerFilterComponent);
        card.add(grid);

        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.SALES));
        add(card);
        grid.loadData();
        search.addClickListener(buttonClickEvent -> {
            grid.loadData();
            offerFilterComponent.fillUrl();
        });
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String params) {
        initView(params);
    }

}
