package cz.bbn.cerberus.contracttype;

import com.vaadin.flow.component.UI;
import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.ContractTypeTabComponent;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.contracttype.ui.ContractTypeDetailView;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class ContractTypeComponentOperation {

    private final ContractTypeService contractTypeService;
    private final AppEnv appEnv;

    public ContractTypeComponentOperation(ContractTypeService contractTypeService, AppEnv appEnv) {
        this.contractTypeService = contractTypeService;
        this.appEnv = appEnv;
    }

    public SaveAction<ContractTypeDto> getSaveAction(AppDialog appDialog) {
        return (newDto, originalDto) -> {
            try {
                if (originalDto != null) {
                    contractTypeService.updateContractType(newDto, originalDto);
                    UI.getCurrent().navigate(AdministrationView.ROUTE.concat("/")
                            .concat(String.valueOf(ContractTypeTabComponent.TAB_INDEX)));

                } else {
                    contractTypeService.saveContractType(newDto);
                    if (appDialog != null) {
                        appDialog.showWarning(false);
                        appDialog.close();
                        UI.getCurrent().navigate(ContractTypeDetailView.ROUTE.concat("/").concat(newDto.getId()));
                    }
                }
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public AppEnv getAppEnv() {
        return appEnv;
    }
}
