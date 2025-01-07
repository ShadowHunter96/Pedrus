package cz.bbn.cerberus.project.ui.component.tab;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.employee.EmployeeComponentOperation;
import cz.bbn.cerberus.employee.ui.component.EmployeeByObjectFilterComponent;
import cz.bbn.cerberus.employee.ui.component.EmployeeByObjectGridComponent;

public class ProjectEmployeeTab extends TabSimpleComponent {

    private final String objectId;
    private final AppEnv appEnv;
    private final EmployeeComponentOperation employeeComponentOperation;

    private EmployeeByObjectGridComponent grid;

    public ProjectEmployeeTab(String objectId, AppEnv appEnv, EmployeeComponentOperation employeeComponentOperation) {
        this.objectId = objectId;
        this.appEnv = appEnv;
        this.employeeComponentOperation = employeeComponentOperation;
        initTab();
    }

    private void initTab() {
        removeAll();

        Button search = VaadinComponents.getSearchButton();
        EmployeeByObjectFilterComponent employeeByObjectFilterComponent = new EmployeeByObjectFilterComponent(search);
        this.add(employeeByObjectFilterComponent);

        grid = new EmployeeByObjectGridComponent(
                employeeComponentOperation.getEmployeeByObjectDeleteAction(objectId, ObjectType.PROJECT), appEnv,
                employeeComponentOperation.getEmployeeByObjectDtoItemsAction(
                        employeeByObjectFilterComponent, objectId, ObjectType.PROJECT));
        this.setSizeFull();
        this.add(grid);

        search.addClickListener(buttonClickEvent ->
                loadTab()
        );
    }

    @Override
    public void loadTab() {
        grid.loadData();
    }

    public EmployeeByObjectGridComponent getGrid() {
        return grid;
    }
}
