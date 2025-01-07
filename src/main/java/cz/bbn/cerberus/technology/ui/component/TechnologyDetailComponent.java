package cz.bbn.cerberus.technology.ui.component;

import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.TechnologyTabComponent;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.appcard.AppDetailCardComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.technology.dto.TechnologyDto;
import cz.bbn.cerberus.technology.ui.component.tab.TechnologyDetailTabComponent;
import cz.bbn.cerberus.translation.Transl;

public class TechnologyDetailComponent
        extends AppDetailCardComponent<TechnologyDto> implements AppBinderOperation<TechnologyDto> {

    public TechnologyDetailComponent(TechnologyDto dto, SaveAction<TechnologyDto> saveAction,
                                     boolean showSubmitButton, AppEnv appEnv,
                                     EntityNewComponentOperation entityNewComponentOperation) {
        super(dto, saveAction, showSubmitButton, appEnv, entityNewComponentOperation);
        initComponent();
    }

    @Override
    protected void initComponent() {
        String heading = Transl.get("Technology")
                .concat(" - ")
                .concat(getDto().getName());
        setHeading(heading);
        this.addBackButton(AdministrationView.ROUTE + "/" + TechnologyTabComponent.TAB_INDEX);
        if (isShowSubmitButton()) {
            this.addSaveButton();
        }
        this.setId(RobotFrameworkVariables.TECHNOLOGY_DETAIL_CARD_ID.getValue());
        this.setSizeFull();

        this.add(new TechnologyDetailTabComponent(this, false));
    }
}
