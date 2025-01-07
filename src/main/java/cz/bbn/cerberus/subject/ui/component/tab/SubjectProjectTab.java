package cz.bbn.cerberus.subject.ui.component.tab;

import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.project.dto.ProjectSimpleDto;
import cz.bbn.cerberus.project.ui.component.ProjectGrid;
import cz.bbn.cerberus.subject.dto.SubjectDto;

import java.util.Map;

public class SubjectProjectTab extends TabSimpleComponent {

    private final AppEnv appEnv;
    private final ItemsAction<ProjectSimpleDto> itemsAction;
    private final Map<String, SubjectDto> subjectMap;
    private ProjectGrid projectGrid;

    public SubjectProjectTab(AppEnv appEnv, ItemsAction<ProjectSimpleDto> itemsAction,
                             Map<String, SubjectDto> subjectMap) {
        this.appEnv = appEnv;
        this.itemsAction = itemsAction;
        this.subjectMap = subjectMap;
        initTab();
    }

    private void initTab() {
        setSizeFull();
        projectGrid = new ProjectGrid(itemsAction, subjectMap, appEnv);
        this.add(projectGrid);
    }

    @Override
    public void loadTab() {
        projectGrid.loadData();
    }

    public AppInfiniteGrid<ProjectSimpleDto> getGrid() {
        return projectGrid;
    }
}
