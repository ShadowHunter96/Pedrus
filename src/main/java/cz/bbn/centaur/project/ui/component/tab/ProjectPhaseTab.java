package cz.bbn.cerberus.project.ui.component.tab;

import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.phase.PhaseComponentOperation;
import cz.bbn.cerberus.phase.ui.component.PhaseGridComponent;

public class ProjectPhaseTab extends TabSimpleComponent {

    private final String projectId;
    private final AppEnv appEnv;
    private final PhaseComponentOperation phaseComponentOperation;
    private final boolean readOnly;

    private PhaseGridComponent grid;

    public ProjectPhaseTab(String projectId, AppEnv appEnv, PhaseComponentOperation phaseComponentOperation,
                           boolean readOnly) {
        this.projectId = projectId;
        this.appEnv = appEnv;
        this.phaseComponentOperation = phaseComponentOperation;
        this.readOnly = readOnly;
        initTab();
    }

    private void initTab() {
        removeAll();
        grid = new PhaseGridComponent(appEnv, phaseComponentOperation.getItemsAction(projectId),
                phaseComponentOperation, readOnly);
        this.setSizeFull();
        this.add(grid);
    }

    @Override
    public void loadTab() {
        grid.loadData();
    }

    public PhaseGridComponent getGrid() {
        return grid;
    }
}
