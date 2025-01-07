package cz.bbn.cerberus.opportunity.ui.component.tabs;

import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.offer.OfferComponentOpperation;
import cz.bbn.cerberus.offer.dto.OfferDto;
import cz.bbn.cerberus.offer.ui.component.OfferTabComponent;

public class OpportunityOfferTab extends TabSimpleComponent {

    private final OfferComponentOpperation offerComponentOpperation;
    private final AppEnv appEnv;
    private final String opportunityId;

    private OfferTabComponent offerTabComponent;

    public OpportunityOfferTab(OfferComponentOpperation offerComponentOpperation, AppEnv appEnv, String opportunityId) {
        this.offerComponentOpperation = offerComponentOpperation;
        this.appEnv = appEnv;
        this.opportunityId = opportunityId;
        initComponent();
    }

    private void initComponent() {
        this.setSizeFull();
        offerTabComponent = new OfferTabComponent(
                offerComponentOpperation, appEnv, opportunityId, ObjectType.OPPORTUNITY);
        this.add(offerTabComponent);
    }

    public AppInfiniteGrid<OfferDto> getGrid() {
        return offerTabComponent.getGrid();
    }
}
