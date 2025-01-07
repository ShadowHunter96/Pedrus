package cz.bbn.cerberus.opportunity.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.contract.ContractComponentOperation;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.contract.dto.ContractInternalType;
import cz.bbn.cerberus.contract.ui.component.ContractSalesDetailComponent;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.opportunity.OpportunityComponentOperation;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.opportunity.factory.OpportunityFactory;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class CreateContractDialogComponent extends AppDialog {

    private final Binder<ContractDto> binder = new Binder<>();

    private final ContractDto contractDto;
    private final OpportunityComponentOperation opportunityComponentOperation;
    private final List<ContractTypeDto> contractTypeList;
    private final AppEnv appEnv;
    private final ListService listService;
    private final ContractComponentOperation contractComponentOperation;

    public CreateContractDialogComponent(OpportunityDto dto,
                                         OpportunityComponentOperation opportunityComponentOperation,
                                         List<ContractTypeDto> contractTypeList, AppEnv appEnv,
                                         ListService listService,
                                         ContractComponentOperation contractComponentOperation) {
        contractDto = OpportunityFactory.opportunityDtoToContractDto(dto);
        this.listService = listService;
        this.opportunityComponentOperation = opportunityComponentOperation;
        this.contractTypeList = contractTypeList;
        this.appEnv = appEnv;
        this.contractComponentOperation = contractComponentOperation;
        init();
    }

    private void init() {
        removeAll();
        setTitle(Transl.get("Create contract from oportunity"));

        Button close = VaadinComponents.getCloseButton();
        close.addClickListener(buttonClickEvent -> this.close());

        ComboBox<ContractDto> connectedContract = new ComboBox<>();

        ContractSalesDetailComponent contractSalesDetailComponent =
                new ContractSalesDetailComponent(contractDto,
                        opportunityComponentOperation.getContractList(), opportunityComponentOperation.getUserList(),
                        appEnv, binder, true, false, null, contractTypeList,
                        listService, connectedContract, ContractInternalType.SALES, contractComponentOperation);
        setContent(contractSalesDetailComponent);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(buttonClickEvent -> {
            ContractDto newDto = binder.getBean();
            if (binder.validate().isOk() && !(newDto.getType() != null && newDto.getType().getConnectionRequired()
                    && newDto.getConnectedContract() == null)) {
                opportunityComponentOperation.getSaveActionCreateContract(this).saveItem(contractDto, null);
            } else if (newDto.getType() != null && newDto.getType().getConnectionRequired()
                    && connectedContract.getValue() == null) {
                connectedContract.setInvalid(true);
            }
            submit.setEnabled(true);
        });
        addCloseButton();
        this.addButtons(submit);
    }
}
