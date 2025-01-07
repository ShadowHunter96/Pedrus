package cz.bbn.cerberus.opportunity.ui.component.tabs;

import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.subject.SubjectComponentOperation;
import cz.bbn.cerberus.subject.ui.component.SubjectGridComponent;
import cz.bbn.cerberus.suppliertype.dto.SupplierTypeDto;

import java.util.Map;

public class OpportunitySubSupplierListTab extends TabSimpleComponent {

    private final SubjectComponentOperation subjectComponentOperation;
    private final String objectId;
    private final AppEnv appEnv;
    private final Map<String, SupplierTypeDto> supplierTypeMap;

    private SubjectGridComponent grid;

    public OpportunitySubSupplierListTab(SubjectComponentOperation subjectComponentOperation, String objectId,
                                         AppEnv appEnv, Map<String, SupplierTypeDto> supplierTypeMap) {
        this.subjectComponentOperation = subjectComponentOperation;
        this.objectId = objectId;
        this.appEnv = appEnv;
        this.supplierTypeMap = supplierTypeMap;
        initTab();
    }

    private void initTab() {
        removeAll();
        grid = new SubjectGridComponent(
                subjectComponentOperation.getDeleteActionSubjectByObject(objectId, ObjectType.OPPORTUNITY),
                subjectComponentOperation.getItemActionSubjectByObjectPage(objectId, ObjectType.OPPORTUNITY),
                appEnv, supplierTypeMap);
        this.setSizeFull();
        this.add(grid);
    }

    @Override
    public void loadTab() {
        grid.loadData();
    }

    public SubjectGridComponent getGrid() {
        return grid;
    }
}
