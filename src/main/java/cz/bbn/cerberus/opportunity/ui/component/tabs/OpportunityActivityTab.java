package cz.bbn.cerberus.opportunity.ui.component.tabs;

import cz.bbn.cerberus.activity.ActivityByObjectComponentOperation;
import cz.bbn.cerberus.activity.ui.component.ActivityByObjectGridComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;

public class OpportunityActivityTab extends TabSimpleComponent {

    private final String objectId;
    private final AppEnv appEnv;
    private final ActivityByObjectComponentOperation activityByObjectComponentOperation;

    private ActivityByObjectGridComponent grid;

    public OpportunityActivityTab(String objectId, AppEnv appEnv,
                                  ActivityByObjectComponentOperation activityByObjectComponentOperation) {
        this.objectId = objectId;
        this.appEnv = appEnv;
        this.activityByObjectComponentOperation = activityByObjectComponentOperation;
        initTab();
    }

    private void initTab() {
        removeAll();
        grid = new ActivityByObjectGridComponent(
                activityByObjectComponentOperation.getDeleteAction(objectId, ObjectType.OPPORTUNITY), appEnv,
                activityByObjectComponentOperation.getItemsAction(objectId, ObjectType.OPPORTUNITY));
        this.setSizeFull();
        this.add(grid);
    }

    @Override
    public void loadTab() {
        grid.loadData();
    }

    public ActivityByObjectGridComponent getGrid() {
        return grid;
    }
}
