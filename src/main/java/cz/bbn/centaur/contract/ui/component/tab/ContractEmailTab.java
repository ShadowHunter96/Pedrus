package cz.bbn.cerberus.contract.ui.component.tab;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.email.EmailComponentOperations;
import cz.bbn.cerberus.email.ui.component.EmailFilterComponent;
import cz.bbn.cerberus.email.ui.component.EmailGridComponent;

public class ContractEmailTab extends TabSimpleComponent {

    private final ContractDto contractDto;
    private final EmailComponentOperations emailComponentOperations;
    private final AppEnv appEnv;

    private EmailGridComponent grid;

    public ContractEmailTab(ContractDto contractDto, EmailComponentOperations emailComponentOperations, AppEnv appEnv) {
        this.contractDto = contractDto;
        this.emailComponentOperations = emailComponentOperations;
        this.appEnv = appEnv;
        initTab();
    }

    private void initTab() {
        Button search = VaadinComponents.getSearchButton();
        EmailFilterComponent filterComponent = new EmailFilterComponent(search, emailComponentOperations,
                DomainEnum.CONTRACT_DOMAIN_NAME, contractDto.getId());

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
