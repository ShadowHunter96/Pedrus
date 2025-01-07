package cz.bbn.cerberus.employee.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.employee.EmployeeComponentOperation;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.employee.ui.component.tab.EmployeeDetailTab;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.translation.Transl;

public class EmployeeNewDialog extends AppDialog {


    private final AppInfiniteGrid<EmployeeDto> grid;
    private final EmployeeComponentOperation employeeComponentOperation;
    private final AppEnv appEnv;
    private final ListService listService;

    public EmployeeNewDialog(AppInfiniteGrid<EmployeeDto> grid, EmployeeComponentOperation employeeComponentOperation,
                             AppEnv appEnv, ListService listService) {
        this.grid = grid;
        this.employeeComponentOperation = employeeComponentOperation;
        this.appEnv = appEnv;
        this.listService = listService;
        init();
    }

    void init() {
        setTitle(Transl.get("New employee"));

        EmployeeDto dto = new EmployeeDto();

        EmployeeDetailTab employeeDetailTab =
                new EmployeeDetailTab(dto, employeeComponentOperation.getEmployeeDtoSaveAction(this),
                        false, appEnv, listService.getUserDtoList());
        employeeDetailTab.setMargin(false);
        employeeDetailTab.setPadding(false);
        setContent(employeeDetailTab);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            employeeDetailTab.saveItem();
            if (grid != null) {
                grid.loadData();
            }
            submit.setEnabled(true);
        });

        showWarning(true);

        addCloseButton();
        addButtons(submit);
    }
}
