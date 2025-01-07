package cz.bbn.cerberus.area.ui.component;

import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.AreaTabComponent;
import cz.bbn.cerberus.area.dto.AreaDto;
import cz.bbn.cerberus.area.ui.component.tab.AreaDetailTabComponent;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.appcard.AppDetailCardComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.translation.Transl;

public class AreaDetailComponent extends AppDetailCardComponent<AreaDto> implements AppBinderOperation<AreaDto> {

    public AreaDetailComponent(AreaDto dto, SaveAction<AreaDto> saveAction, boolean showSubmitButton, AppEnv appEnv,
                               EntityNewComponentOperation entityNewComponentOperation) {
        super(dto, saveAction, showSubmitButton, appEnv, entityNewComponentOperation);
        initComponent();
    }

    @Override
    protected void initComponent() {
        String heading = Transl.get("Area")
                .concat(" - ")
                .concat(getDto().getName());
        setHeading(heading);
        this.addBackButton(AdministrationView.ROUTE + "/" + AreaTabComponent.TAB_INDEX);
        if (isShowSubmitButton()) {
            this.addSaveButton();
        }
        this.setId(RobotFrameworkVariables.AREA_DETAIL_CARD_ID.getValue());
        this.setSizeFull();

        this.add(new AreaDetailTabComponent(this, false));
    }
}
