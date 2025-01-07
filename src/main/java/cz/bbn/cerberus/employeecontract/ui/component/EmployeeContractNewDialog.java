package cz.bbn.cerberus.employeecontract.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.employeecontract.EmployeeContractComponentOperation;
import cz.bbn.cerberus.employeecontract.dto.EmployeeContractDto;
import cz.bbn.cerberus.employeecontract.ui.component.tab.EmployeeContractDetailTab;
import cz.bbn.cerberus.translation.Transl;

public class EmployeeContractNewDialog extends AppDialog {

    private final AppInfiniteGrid<EmployeeContractDto> grid;
    private final EmployeeContractComponentOperation componentOperation;
    private final AppEnv appEnv;

    private EmployeeContractDto dto;

    public EmployeeContractNewDialog(AppInfiniteGrid<EmployeeContractDto> grid,
                                     EmployeeContractComponentOperation componentOperation, AppEnv appEnv) {
        this.grid = grid;
        this.dto = null;
        this.componentOperation = componentOperation;
        this.appEnv = appEnv;
        initDialog();
    }

    public EmployeeContractNewDialog(EmployeeContractDto dto,
                                     EmployeeContractComponentOperation componentOperation, AppEnv appEnv) {
        this.grid = null;
        this.dto = dto;
        this.componentOperation = componentOperation;
        this.appEnv = appEnv;
        initDialog();
    }

    private void initDialog() {
        setTitle(Transl.get("New employee contract"));

        if (dto == null) {
            dto = new EmployeeContractDto();
        }

        dto.setReminder(30);
        dto.setOwnCompany(componentOperation.getOwnCompany());

        EmployeeContractDetailTab employeeContractDetailTab =
                new EmployeeContractDetailTab(dto, componentOperation.getSaveAction(this),
                        componentOperation.getOwnCompanyList(), componentOperation.getTypeList(),
                        componentOperation.getEmployeeList(), componentOperation.getEmpConListExceptId(dto.getId()),
                        componentOperation, componentOperation.getStateList(), appEnv);
        setContent(employeeContractDetailTab);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            employeeContractDetailTab.saveItem();
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
