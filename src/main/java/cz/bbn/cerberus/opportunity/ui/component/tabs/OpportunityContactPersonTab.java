package cz.bbn.cerberus.opportunity.ui.component.tabs;

import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.contactperson.ui.components.ContactPersonAddToGridComponent;
import cz.bbn.cerberus.opportunity.OpportunityComponentOperation;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.permission.Permission;

public class OpportunityContactPersonTab extends TabSimpleComponent {

    private final OpportunityComponentOperation opportunityComponentOperation;
    private final AppEnv appEnv;
    private ContactPersonAddToGridComponent grid;
    private final OpportunityDto dto;

    public OpportunityContactPersonTab(
            OpportunityComponentOperation opportunityComponentOperation, OpportunityDto dto, AppEnv appEnv) {
        this.opportunityComponentOperation = opportunityComponentOperation;
        this.appEnv = appEnv;
        this.dto = dto;
        initTab();
    }

    private void initTab() {
        setSizeFull();
        grid = new ContactPersonAddToGridComponent(
                opportunityComponentOperation.getContactPersonRemoveAction(this, dto),
                opportunityComponentOperation.getContactPersonItemsAction(dto), Permission.OPPORTUNITY_EDIT, appEnv);

        grid.setSizeFull();
        add(grid);
        grid.loadData();
    }

    public ContactPersonAddToGridComponent getGrid() {
        return grid;
    }
}
