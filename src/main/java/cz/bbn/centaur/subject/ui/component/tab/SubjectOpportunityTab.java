package cz.bbn.cerberus.subject.ui.component.tab;

import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.opportunity.dto.OpportunitySimpleDto;
import cz.bbn.cerberus.opportunity.ui.component.OpportunityGridComponent;
import cz.bbn.cerberus.subject.SubjectComponentOperation;
import cz.bbn.cerberus.subject.dto.SubjectDto;

import java.util.Map;

public class SubjectOpportunityTab extends TabSimpleComponent {

    private OpportunityGridComponent grid;

    private final ItemsAction<OpportunitySimpleDto> itemsAction;
    private final AppEnv appEnv;
    private final Map<String, SubjectDto> subjectMap;
    private final SubjectComponentOperation subjectComponentOperation;

    public SubjectOpportunityTab(ItemsAction<OpportunitySimpleDto> itemsAction, AppEnv appEnv, Map<String,
            SubjectDto> subjectMap, SubjectComponentOperation subjectComponentOperation) {
        this.itemsAction = itemsAction;
        this.appEnv = appEnv;
        this.subjectMap = subjectMap;
        this.subjectComponentOperation = subjectComponentOperation;
        initTab();
    }

    private void initTab() {
        removeAll();
        grid = new OpportunityGridComponent(appEnv, itemsAction, subjectMap, subjectComponentOperation.getOwnerMap());
        this.setSizeFull();
        this.add(grid);
    }

    @Override
    public void loadTab() {
        grid.loadData();
    }

    public AppInfiniteGrid<OpportunitySimpleDto> getGrid() {
        return grid;
    }
}
