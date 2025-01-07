package cz.bbn.cerberus.contract.ui.component.tab;

import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import cz.bbn.cerberus.areatechnologysign.ui.AreaTechnologySignsBadgeComponent;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabDtoComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.contract.ContractComponentOperation;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.contract.dto.ContractInternalType;
import cz.bbn.cerberus.contract.ui.component.ContractBackOfficeDetailComponent;
import cz.bbn.cerberus.contract.ui.component.ContractSalesDetailComponent;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.user.dto.UserDto;

import java.util.List;

public class ContractDetailTab extends TabDtoComponent<ContractDto> {

    private final AppEnv appEnv;
    private final List<ContractDto> contractList;
    private final List<UserDto> userList;
    private final boolean isDialog;
    private final boolean readOnly;
    private final AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent;
    private final List<ContractTypeDto> contractTypeList;
    private final ListService listService;
    private final ContractComponentOperation componentOperation;

    private final ComboBox<ContractDto> connectedContract;

    public ContractDetailTab(ContractDto dto, SaveAction<ContractDto> saveAction, AppEnv appEnv,
                             List<ContractDto> contractList, List<UserDto> userList,
                             boolean isDialog, boolean readOnly, List<ContractTypeDto> contractTypeList,
                             AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent,
                             ListService listService, ComboBox<ContractDto> connectedContract,
                             ContractComponentOperation componentOperation) {

        super(dto, saveAction, appEnv);
        this.appEnv = appEnv;
        this.contractList = contractList;
        this.userList = userList;
        this.isDialog = isDialog;
        this.readOnly = readOnly;
        this.contractTypeList = contractTypeList;
        this.areaTechnologySignsBadgeComponent = areaTechnologySignsBadgeComponent;
        this.listService = listService;
        this.connectedContract = connectedContract;
        this.componentOperation = componentOperation;
        initTab();
    }

    @Override
    protected void initTab() {
        removeAll();

        this.setId(RobotFrameworkVariables.CONTRACT_DETAIL_CARD_ID.getValue());
        this.setSizeFull();
        this.setMargin(false);
        this.setPadding(false);

        HorizontalLayout mainHorizontalLayout = new HorizontalLayout();
        mainHorizontalLayout.setSizeFull();

        if (getDto().getInternalType() == ContractInternalType.SALES) {
            ContractSalesDetailComponent contractSalesDetailComponent = new ContractSalesDetailComponent(
                    getDto(), contractList, userList, appEnv, getBinder(), isDialog, readOnly,
                    areaTechnologySignsBadgeComponent, contractTypeList, listService, connectedContract,
                    ContractInternalType.SALES, componentOperation);
            mainHorizontalLayout.add(contractSalesDetailComponent);
        }

        if (getDto().getInternalType() == ContractInternalType.SUPPLIER) {
            ContractSalesDetailComponent contractSalesDetailComponent = new ContractSalesDetailComponent(
                    getDto(), contractList, userList, appEnv, getBinder(), isDialog, readOnly,
                    areaTechnologySignsBadgeComponent, contractTypeList, listService, connectedContract,
                    ContractInternalType.SUPPLIER, componentOperation);
            mainHorizontalLayout.add(contractSalesDetailComponent);
        }

        if (getDto().getInternalType() == ContractInternalType.OPERATIONAL) {
            ContractBackOfficeDetailComponent contractBackOfficeDetailComponent = new ContractBackOfficeDetailComponent(
                    getDto(), contractList, userList, appEnv, getBinder(), isDialog, readOnly,
                    areaTechnologySignsBadgeComponent, contractTypeList, listService,
                    connectedContract, componentOperation);
            mainHorizontalLayout.add(contractBackOfficeDetailComponent);
        }

        this.add(mainHorizontalLayout);
    }
}
