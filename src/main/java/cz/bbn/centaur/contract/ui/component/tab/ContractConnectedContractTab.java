package cz.bbn.cerberus.contract.ui.component.tab;

import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.contract.ui.component.ContractGridComponent;

public class ContractConnectedContractTab extends TabSimpleComponent {

    private final ItemsAction<ContractDto> itemsAction;
    private final AppEnv appEnv;

    private ContractGridComponent contractGridComponent;

    public ContractConnectedContractTab(ItemsAction<ContractDto> itemsAction, AppEnv appEnv) {
        this.itemsAction = itemsAction;
        this.appEnv = appEnv;
        initTab();
    }

    private void initTab() {
        this.setSizeFull();
        contractGridComponent = new ContractGridComponent(itemsAction, appEnv);
        contractGridComponent.loadData();
        this.add(contractGridComponent);
    }

    @Override
    public void loadTab() {
        contractGridComponent.loadData();
    }
}
