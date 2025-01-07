package cz.bbn.cerberus.suppliertype.ui.component;

import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.SupplierTypeTabsComponent;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.appcard.AppDetailCardComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.suppliertype.dto.SupplierTypeDto;
import cz.bbn.cerberus.suppliertype.ui.component.tab.SupplierTypeDetailTabComponent;
import cz.bbn.cerberus.translation.Transl;

public class SupplierTypeDetailComponent
        extends AppDetailCardComponent<SupplierTypeDto> implements AppBinderOperation<SupplierTypeDto> {

    public SupplierTypeDetailComponent(SupplierTypeDto dto, SaveAction<SupplierTypeDto> saveAction,
                                       boolean showSubmitButton, AppEnv appEnv,
                                       EntityNewComponentOperation entityNewComponentOperation) {
        super(dto, saveAction, showSubmitButton, appEnv, entityNewComponentOperation);
        initComponent();
    }

    @Override
    protected void initComponent() {
        String heading = getDto().getId() == null ? Transl.get("New supplier type") :
                Transl.get("Supplier type")
                        .concat(" - ")
                        .concat(getDto().getName());
        setHeading(heading);
        this.addBackButton(AdministrationView.ROUTE + "/" + SupplierTypeTabsComponent.TAB_INDEX);
        if (isShowSubmitButton()) {
            this.addSaveButton();
        }
        this.setId(RobotFrameworkVariables.SUPPLIER_TYPE_DETAIL_CARD_ID.getValue());
        this.setSizeFull();

        this.add(new SupplierTypeDetailTabComponent(this, false));
    }
}
