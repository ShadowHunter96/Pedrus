package cz.bbn.cerberus.administration.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.suppliertype.SupplierTypeComponentOperation;
import cz.bbn.cerberus.suppliertype.SupplierTypeService;
import cz.bbn.cerberus.suppliertype.dto.SupplierTypeDto;
import cz.bbn.cerberus.suppliertype.ui.component.SupplierTypeGridComponent;
import cz.bbn.cerberus.suppliertype.ui.component.SupplierTypeNewDialog;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class SupplierTypeTabsComponent extends TabSimpleComponent {

    public static final int TAB_INDEX = 3;

    private final AppEnv appEnv;
    private final SupplierTypeService supplierTypeService;
    private final SupplierTypeComponentOperation supplierTypeComponentOperation;

    private SupplierTypeGridComponent supplierTypeGridComponent;

    public SupplierTypeTabsComponent(AppEnv appEnv, SupplierTypeService supplierTypeService,
                                     SupplierTypeComponentOperation supplierTypeComponentOperation) {
        this.appEnv = appEnv;
        this.supplierTypeService = supplierTypeService;
        this.supplierTypeComponentOperation = supplierTypeComponentOperation;
        initComponent();
    }

    public void initComponent() {
        setSizeFull();
        setId(RobotFrameworkVariables.SUPPLIER_TYPE_TAB_CARD_ID.getValue());

        supplierTypeGridComponent = new SupplierTypeGridComponent(getDeleteAction(), appEnv, getItemsAction(),
                supplierTypeComponentOperation.getListAction(), getListActionForChange(),
                supplierTypeComponentOperation.getSaveActionChange());
        supplierTypeGridComponent.loadData();
        add(supplierTypeGridComponent);

    }

    @Override
    public Button getFooterButton() {
        if (SecurityUtils.hasPermission(Permission.SUPPLIER_TYPE_EDIT)) {
            Button addNew = VaadinComponents.getNewButton(Transl.get("Add supplier type"));
            addNew.addClickListener(e ->
                    new SupplierTypeNewDialog(supplierTypeGridComponent, supplierTypeComponentOperation).open());
            return addNew;
        }
        return null;
    }

    private ListAction<SupplierTypeDto> getListActionForChange() {
        return supplierTypeService::findAllAllowedExceptOne;
    }

    private ItemsAction<SupplierTypeDto> getItemsAction() {
        return (query, orderList) ->
                supplierTypeService.findSupplierTypeDtoPage(query.getPage(), query.getPageSize(), orderList);
    }


    private DeleteAction getDeleteAction() {
        return id -> {
            try {
                supplierTypeService.deleteSupplierType(id);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }
}
