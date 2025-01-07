package cz.bbn.cerberus.subject.ui.component.tab;

import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.offer.OfferComponentOpperation;
import cz.bbn.cerberus.offer.dto.OfferDto;
import cz.bbn.cerberus.offer.ui.component.OfferTabComponent;

public class SubjectOfferTab extends TabSimpleComponent {

    private final OfferComponentOpperation offerComponentOpperation;
    private final AppEnv appEnv;
    private final String subjectId;

    private OfferTabComponent offerTabComponent;

    public SubjectOfferTab(OfferComponentOpperation offerComponentOpperation, AppEnv appEnv, String subjectId) {
        this.offerComponentOpperation = offerComponentOpperation;
        this.appEnv = appEnv;
        this.subjectId = subjectId;
        initComponent();
    }

    private void initComponent(){
        this.setSizeFull();
        offerTabComponent = new OfferTabComponent(offerComponentOpperation, appEnv, subjectId, ObjectType.SUBJECT);
        this.add(offerTabComponent);
    }

    public AppInfiniteGrid<OfferDto> getGrid() {
        return offerTabComponent.getGrid();
    }
}
