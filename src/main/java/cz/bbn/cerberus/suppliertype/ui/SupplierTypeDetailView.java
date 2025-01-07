package cz.bbn.cerberus.suppliertype.ui;

import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.suppliertype.SupplierTypeComponentOperation;
import cz.bbn.cerberus.suppliertype.SupplierTypeService;
import cz.bbn.cerberus.suppliertype.dto.SupplierTypeDto;
import cz.bbn.cerberus.suppliertype.ui.component.SupplierTypeDetailComponent;
import lombok.extern.slf4j.Slf4j;

@Route(value = SupplierTypeDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.SUPPLIER_TYPE_VIEW)
@Slf4j
public class SupplierTypeDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "supplier-type-detail";

    private final SupplierTypeService supplierTypeService;
    private final SupplierTypeComponentOperation supplierTypeComponentOperation;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public SupplierTypeDetailView(SupplierTypeService supplierTypeService,
                                  SupplierTypeComponentOperation supplierTypeComponentOperation,
                                  AppEnv appEnv, EntityNewComponentOperation entityNewComponentOperation) {
        this.supplierTypeService = supplierTypeService;
        this.supplierTypeComponentOperation = supplierTypeComponentOperation;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(SupplierTypeDto dto) {
        removeAll();
        setSizeFull();
        SupplierTypeDetailComponent supplierTypeDetailTypeComponent =
                new SupplierTypeDetailComponent(
                        dto,
                        supplierTypeComponentOperation.getSaveAction(null),
                        SecurityUtils.hasPermission(Permission.SUPPLIER_TYPE_EDIT),
                        appEnv, entityNewComponentOperation);
        add(supplierTypeDetailTypeComponent);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String param) {
        if (param != null) {
            try {
                SupplierTypeDto dto = supplierTypeService.getSupplierType(param);
                refreshBreadcrumbText(dto.getId());
                initView(dto);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        }
    }
}
