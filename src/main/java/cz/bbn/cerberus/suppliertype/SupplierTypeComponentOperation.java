package cz.bbn.cerberus.suppliertype;

import com.vaadin.flow.component.UI;
import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.SupplierTypeTabsComponent;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.subject.SubjectService;
import cz.bbn.cerberus.suppliertype.dto.SupplierTypeDto;
import cz.bbn.cerberus.suppliertype.ui.SupplierTypeDetailView;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class SupplierTypeComponentOperation {

    private final SupplierTypeService supplierTypeService;
    private final AppEnv appEnv;
    private final SubjectService subjectService;

    public SupplierTypeComponentOperation(SupplierTypeService supplierTypeService, AppEnv appEnv,
                                          SubjectService subjectService) {
        this.supplierTypeService = supplierTypeService;
        this.appEnv = appEnv;
        this.subjectService = subjectService;
    }

    public ListAction<String> getListAction() {
        return subjectService::getIdListBySupplierTypeId;
    }

    public SaveAction<String> getSaveActionChange() {
        return (newValue, oldValue) -> {
            try {
                subjectService.changeSupplierType(newValue, oldValue);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    public SaveAction<SupplierTypeDto> getSaveAction(AppDialog dialog) {
        return (dto, originalDto) -> {
            try {
                if (supplierTypeService.supplierTypeExists(dto.getId())) {
                    supplierTypeService.updateSupplierType(dto, originalDto);
                    UI.getCurrent().navigate(AdministrationView.ROUTE.concat("/")
                            .concat(String.valueOf(SupplierTypeTabsComponent.TAB_INDEX)));
                } else {
                    supplierTypeService.saveSupplierType(dto);
                    if (dialog != null) {
                        dialog.showWarning(false);
                        dialog.close();
                        UI.getCurrent().navigate(SupplierTypeDetailView.ROUTE.concat("/").concat(dto.getId()));
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
