package cz.bbn.cerberus.dph.ui.component;

import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.DphTabComponent;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.appcard.AppDetailCardComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.dph.dto.DphDto;
import cz.bbn.cerberus.dph.ui.component.tab.DphDetailTabComponent;
import cz.bbn.cerberus.translation.Transl;

public class DphDetailComponent extends AppDetailCardComponent<DphDto> implements AppBinderOperation<DphDto> {

    public DphDetailComponent(DphDto dto, SaveAction<DphDto> saveAction, boolean showSubmitButton, AppEnv appEnv,
                              EntityNewComponentOperation entityNewComponentOperation) {
        super(dto, saveAction, showSubmitButton, appEnv, entityNewComponentOperation);
        initComponent();
    }

    @Override
    protected void initComponent() {
        String heading = Transl.get("Dph")
                .concat(" - ")
                .concat(getDto().getName());
        setHeading(heading);
        this.addBackButton(AdministrationView.ROUTE + "/" + DphTabComponent.TAB_INDEX);
        if (isShowSubmitButton()) {
            this.addSaveButton();
        }
        this.setId(RobotFrameworkVariables.DPH_DETAIL_CARD_ID.getValue());
        this.setSizeFull();

        this.add(new DphDetailTabComponent(this, false));
    }
}
