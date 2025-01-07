package cz.bbn.cerberus.contracttype.ui;

import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contracttype.ContractTypeComponentOperation;
import cz.bbn.cerberus.contracttype.ContractTypeService;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.contracttype.ui.components.ContractTypeDetailComponent;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import lombok.extern.slf4j.Slf4j;

@Route(value = ContractTypeDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.CONTRACT_TYPE_VIEW)
@Slf4j
public class ContractTypeDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "contract-type-detail";

    private final ContractTypeService contractTypeService;
    private final ContractTypeComponentOperation contractTypeComponentOperation;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public ContractTypeDetailView(ContractTypeService contractTypeService,
                                  ContractTypeComponentOperation contractTypeComponentOperation,
                                  AppEnv appEnv, EntityNewComponentOperation entityNewComponentOperation) {
        this.contractTypeService = contractTypeService;
        this.contractTypeComponentOperation = contractTypeComponentOperation;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(ContractTypeDto dto) {
        ContractTypeDetailComponent contractTypeDetailComponent =
                new ContractTypeDetailComponent(dto, contractTypeComponentOperation.getSaveAction(null),
                        SecurityUtils.hasPermission(Permission.CONTRACT_TYPE_EDIT),
                        appEnv, entityNewComponentOperation);
        this.add(contractTypeDetailComponent);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, String param) {
        try {
            ContractTypeDto dto = contractTypeService.getContractType(param);
            refreshBreadcrumbText(dto.getId());
            initView(dto);
        } catch (SystemException ex) {
            log.error(TextValues.SYSTEM_EXCEPTION, ex);
            ErrorNotification.show(ex, appEnv);
        }
    }
}
