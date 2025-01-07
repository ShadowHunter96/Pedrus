package cz.bbn.cerberus.employeecontract.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.employeecontract.EmployeeContractComponentOperation;
import cz.bbn.cerberus.employeecontract.dto.EmployeeContractDto;
import cz.bbn.cerberus.translation.Transl;

public class EmployeeContractConfirmDeleteDialog extends AppDialog {

    private final EmployeeContractDto dto;
    private final EmployeeContractComponentOperation componentOperation;

    public EmployeeContractConfirmDeleteDialog(EmployeeContractDto dto,
                                               EmployeeContractComponentOperation componentOperation) {
        this.dto = dto;
        this.componentOperation = componentOperation;
        initDialog();
    }

    private void initDialog() {
        setTitle(Transl.get("Delete employee contract"));

        TextField confirmDelete = new TextField(Transl.get("Type contract name to confirm delete"));

        setContent(confirmDelete);

        addCloseButton();

        Button submitButton = VaadinComponents.getSubmitButton();

        submitButton.addClickListener(e -> {
            if (dto.getName() != null && dto.getName().equals(confirmDelete.getValue())) {
                componentOperation.deleteEmpContract(dto, this);
            } else {
                confirmDelete.setErrorMessage(Transl.get("Typed name does not match"));
                confirmDelete.setInvalid(true);
            }
        });

        addButtons(submitButton);
    }
}
