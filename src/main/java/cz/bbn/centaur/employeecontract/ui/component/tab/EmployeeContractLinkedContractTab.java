package cz.bbn.cerberus.employeecontract.ui.component.tab;

import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.employeecontract.dto.EmployeeContractDto;
import cz.bbn.cerberus.employeecontract.ui.component.EmployeeContractGridComponent;

public class EmployeeContractLinkedContractTab extends TabSimpleComponent {

    private final ItemsAction<EmployeeContractDto> itemsAction;
    private final AppEnv appEnv;

    private EmployeeContractGridComponent contractGridComponent;

    public EmployeeContractLinkedContractTab(ItemsAction<EmployeeContractDto> itemsAction, AppEnv appEnv) {
        this.itemsAction = itemsAction;
        this.appEnv = appEnv;
        initTab();
    }

    private void initTab() {
        this.setSizeFull();
        contractGridComponent = new EmployeeContractGridComponent(appEnv, itemsAction);
        contractGridComponent.loadData();
        this.add(contractGridComponent);
    }

    @Override
    public void loadTab() {
        contractGridComponent.loadData();
    }
}
