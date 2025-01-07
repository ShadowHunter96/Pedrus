package cz.bbn.cerberus.administration.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.ContractService;
import cz.bbn.cerberus.contracttype.ContractTypeComponentOperation;
import cz.bbn.cerberus.contracttype.ContractTypeService;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.contracttype.ui.components.ContractTypeGridComponent;
import cz.bbn.cerberus.contracttype.ui.components.ContractTypeNewDialog;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ContractTypeTabComponent extends TabSimpleComponent {

    public static final int TAB_INDEX = 0;

    private final ContractService contractService;
    private final ContractTypeService contractTypeService;
    private final ContractTypeComponentOperation contractTypeComponentOperation;
    private final AppEnv appEnv;

    private ContractTypeGridComponent contractTypeGridComponent;

    public ContractTypeTabComponent(AppEnv appEnv, ContractTypeService contractTypeService,
                                    ContractTypeComponentOperation contractTypeComponentOperation,
                                    ContractService contractService) {
        this.contractTypeService = contractTypeService;
        this.contractTypeComponentOperation = contractTypeComponentOperation;
        this.contractService = contractService;
        this.appEnv = appEnv;
        initComponent();
    }

    private void initComponent() {
        setSizeFull();
        setId(RobotFrameworkVariables.CONTRACT_TYPE_TAB_CARD_ID.getValue());
        contractTypeGridComponent = new ContractTypeGridComponent(getDeleteAction(), appEnv, getItemsAction(),
                getListAction(), getListActionForChange(), getSaveActionChange());

        contractTypeGridComponent.loadData();
        this.add(contractTypeGridComponent);

    }

    private ListAction<String> getListAction() {
        return contractService::getUsedContractType;
    }

    private ListAction<ContractTypeDto> getListActionForChange() {
        return contractTypeService::findAllEnabled;
    }

    private SaveAction<String> getSaveActionChange() {
        return (newValue, oldValue) -> {
            try {
                contractService.changeContractType(newValue, oldValue);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    @Override
    public Button getFooterButton() {
        if (SecurityUtils.hasPermission(Permission.CONTRACT_TYPE_EDIT)) {
            Button addNew = VaadinComponents.getNewButton(Transl.get("Add contract type"));
            addNew.addClickListener(e -> new ContractTypeNewDialog(
                    contractTypeGridComponent, contractTypeComponentOperation).open());
            return addNew;
        }
        return null;
    }

    private DeleteAction getDeleteAction() {
        return id -> {
            try {
                contractTypeService.deleteContractType(id);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    private ItemsAction<ContractTypeDto> getItemsAction() {
        return (query, orderList) ->
                contractTypeService.findContractTypeDtoPage(query.getPage(), query.getPageSize(), orderList);
    }
}
