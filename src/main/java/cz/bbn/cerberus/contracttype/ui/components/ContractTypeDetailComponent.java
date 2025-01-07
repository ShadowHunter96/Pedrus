package cz.bbn.cerberus.contracttype.ui.components;

import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.ContractTypeTabComponent;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.appcard.AppDetailCardComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.contracttype.ui.components.tab.ContractTypeDetailTabComponent;
import cz.bbn.cerberus.translation.Transl;

public class ContractTypeDetailComponent extends AppDetailCardComponent<ContractTypeDto>
        implements AppBinderOperation<ContractTypeDto> {

    public ContractTypeDetailComponent(ContractTypeDto dto, SaveAction<ContractTypeDto> saveAction,
                                       boolean showSubmitButton, AppEnv appEnv,
                                       EntityNewComponentOperation entityNewComponentOperation) {
        super(dto, saveAction, showSubmitButton, appEnv, entityNewComponentOperation);
        initComponent();
    }

    @Override
    protected void initComponent() {
        String heading = getDto().getId() == null ? Transl.get("New contract type") :
                Transl.get("Contract type")
                        .concat(" - ")
                        .concat(getDto().getName());
        setHeading(heading);
        this.addBackButton(AdministrationView.ROUTE + "/" + ContractTypeTabComponent.TAB_INDEX);
        if (isShowSubmitButton()) {
            this.addSaveButton();
        }
        this.setId(RobotFrameworkVariables.CONTRACT_TYPE_DETAIL_CARD_ID.getValue());
        this.setSizeFull();

        this.add(new ContractTypeDetailTabComponent(this, false));
    }
}
