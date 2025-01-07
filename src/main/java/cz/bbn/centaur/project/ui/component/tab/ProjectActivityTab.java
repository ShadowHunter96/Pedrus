package cz.bbn.cerberus.project.ui.component.tab;

import cz.bbn.cerberus.activity.ActivityByObjectComponentOperation;
import cz.bbn.cerberus.activity.ui.component.ActivityByObjectGridComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;

public class ProjectActivityTab extends TabSimpleComponent {

    private final String objectId;
    private final AppEnv appEnv;
    private final ActivityByObjectComponentOperation activityByObjectComponentOperation;

    private ActivityByObjectGridComponent grid;

    public ProjectActivityTab(String objectId, AppEnv appEnv,
                              ActivityByObjectComponentOperation activityByObjectComponentOperation) {
        this.objectId = objectId;
        this.appEnv = appEnv;
        this.activityByObjectComponentOperation = activityByObjectComponentOperation;
        initTab();
    }

    private void initTab() {
        removeAll();
        grid = new ActivityByObjectGridComponent(
                activityByObjectComponentOperation.getDeleteAction(objectId, ObjectType.PROJECT), appEnv,
                activityByObjectComponentOperation.getItemsAction(objectId, ObjectType.PROJECT));
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
