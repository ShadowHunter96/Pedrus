package cz.bbn.cerberus.offer.ui.component.tabs;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.email.EmailComponentOperations;
import cz.bbn.cerberus.email.ui.component.EmailFilterComponent;
import cz.bbn.cerberus.email.ui.component.EmailGridComponent;
import cz.bbn.cerberus.offer.dto.OfferDto;

public class OfferEmailTab extends TabSimpleComponent {

    private final OfferDto offerDto;
    private final EmailComponentOperations emailComponentOperations;
    private final AppEnv appEnv;

    private EmailGridComponent grid;

    public OfferEmailTab(OfferDto offerDto, EmailComponentOperations emailComponentOperations, AppEnv appEnv) {
        this.offerDto = offerDto;
        this.emailComponentOperations = emailComponentOperations;
        this.appEnv = appEnv;
        initTab();
    }

    private void initTab() {
        Button search = VaadinComponents.getSearchButton();
        EmailFilterComponent filterComponent = new EmailFilterComponent(search, emailComponentOperations,
                DomainEnum.OFFER_DOMAIN_NAME, offerDto.getId());

        grid = new EmailGridComponent(emailComponentOperations.getDeleteAction(), appEnv,
                emailComponentOperations.getItemsAction(filterComponent), emailComponentOperations);

        search.addClickListener(e -> grid.loadData());
        this.add(filterComponent, grid);
        this.setSizeFull();
    }

    @Override
    public void loadTab() {
        grid.loadData();
    }
}
