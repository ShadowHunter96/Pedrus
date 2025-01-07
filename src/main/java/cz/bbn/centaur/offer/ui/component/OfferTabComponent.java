package cz.bbn.cerberus.offer.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.offer.OfferComponentOpperation;
import cz.bbn.cerberus.offer.dto.OfferDto;

public class OfferTabComponent extends VerticalLayout {

    private final OfferComponentOpperation offerComponentOpperation;
    private final AppEnv appEnv;
    private final String objectId;
    private final ObjectType objectType;

    private OfferGridComponent grid;
    private OfferFilterComponent filterComponent;

    public OfferTabComponent(OfferComponentOpperation offerComponentOpperation, AppEnv appEnv,
                             String objectId, ObjectType objectType) {
        this.offerComponentOpperation = offerComponentOpperation;
        this.appEnv = appEnv;
        this.objectId = objectId;
        this.objectType = objectType;
        initComponent();
    }

    private void initComponent() {
        this.setSizeFull();
        this.setPadding(false);
        this.setMargin(false);
        Button search = VaadinComponents.getSearchButton();

        filterComponent = new OfferFilterComponent(search, objectId, objectType);
        grid = new OfferGridComponent(offerComponentOpperation.getDeleteAction(), appEnv,
                offerComponentOpperation.getItemsAction(filterComponent), objectType == ObjectType.SUBJECT ? 1 : 0);
        filterComponent.setGrid(grid);
        this.add(filterComponent);

        grid.loadData();
        search.addClickListener(buttonClickEvent -> grid.loadData());
        this.add(grid);
    }

    public AppInfiniteGrid<OfferDto> getGrid() {
        return grid;
    }
}
