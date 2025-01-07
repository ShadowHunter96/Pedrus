package cz.bbn.cerberus.employeecontract.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.employeecontract.EmployeeContractComponentOperation;
import cz.bbn.cerberus.employeecontract.dto.EmployeeContractDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class EmployeeContractLinkContractDialog extends AppDialog {

    private final List<EmployeeContractDto> empContractList;
    private final EmployeeContractComponentOperation componentOperation;
    private final EmployeeContractDto originalDto;

    public EmployeeContractLinkContractDialog(List<EmployeeContractDto> empContractList,
                                              EmployeeContractComponentOperation componentOperation,
                                              EmployeeContractDto originalDto) {
        this.empContractList = empContractList;
        this.componentOperation = componentOperation;
        this.originalDto = originalDto;
        initDialog();
    }

    private void initDialog() {
        setTitle("Link employee contract");

        ComboBox<EmployeeContractDto> linkedContract = new ComboBox<>();
        linkedContract.setItems(empContractList);
        linkedContract.setItemLabelGenerator(EmployeeContractDto::getName);
        setContent(linkedContract);

        addCloseButton();

        Button submit = VaadinComponents.getSubmitButton();
        submit.addClickListener(e -> {
            if (linkedContract.getValue() != null) {
                componentOperation.linkContract(originalDto, linkedContract.getValue(), this);
            } else {
                linkedContract.setErrorMessage(Transl.get(TextValues.CANNOT_BE_EMPTY));
                linkedContract.setInvalid(true);
            }
        });
        addSubmitButton(submit);
    }
}
